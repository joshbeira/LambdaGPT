module LGPT.TUI where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import LGPT.Helpers (Parser, prompt, runStart)
import LGPT.Numbers (parseLonghand, printLonghand)
import Data.Time (getCurrentTime, utctDay, dayOfWeek, addDays, diffDays, fromGregorian, Day)

--------------------------------------------------------------------------------

runREPL :: IO ()
runREPL = loop [] -- Start the chatbot with an empty memory list
  where
    loop :: [(String, String)] -> IO ()
    loop memory = do
      putStr prompt
      req <- getLine
      -- pass the current memory into respondTo and catch the updated memory it returns
      newMemory <- respondTo memory (readRequest req)
      loop newMemory -- Loop again using the new memory
--------------------------------------------------------------------------------
-- Parsing and responding to requests:


-- | Our request type, the result of parsing a string.
data Request = Unknown | Hello | WhatDay | WhatDayTomorrow | HowLongAgo Day | WhatIs Expr| Remember String String | TellMeAbout String
  deriving (Eq, Show)

{- | Read a request. 

    This runs the parse function from Megaparsec, and
    converts any failed parses into an Unknown request.
-}
readRequest :: String -> Request
readRequest str = case parse parseRequest "<stdin>" str of
  Left  err -> Unknown
  Right req -> req


-- | Currently, the only thing λGPT understands is "Hello"...
parseRequest :: Parser Request
parseRequest = choice
  [ do
      _ <- string "Hello"
      pure Hello
  , do
      _ <- string "What day is it tomorrow?" -- "what day is it tomorrow" has to come before "what day is it"
      pure WhatDayTomorrow
  , do
      _ <- string "What day is it?"
      pure WhatDay
  , do
        _ <- string "How long ago was "
        year <- some digitChar
        _ <- char '-'
        month <- some digitChar
        _ <- char '-'
        day <- some digitChar
        _ <- string "?" <|> string "" -- catch the question mark if they typed it

        let targetDate = fromGregorian (read year) (read month) (read day)
        pure (HowLongAgo targetDate)
  , do
      _ <- string "What is "
      expr <- parseExpr
      _ <- string "?" <|> string "" -- catch the question mark if they typed it
      pure (WhatIs expr)
  , do
      _ <- string "Remember that " -- grab characters until we hit " is "
      name <- someTill anySingle (string " is ") -- The rest of the string is the thing
      thing <- some anySingle
      pure (Remember name thing)
  , do
      _ <- string "Tell me about " -- grab the name, stopping if they typed a period at the end
      name <- someTill anySingle (char '.') <|> some anySingle
      pure (TellMeAbout name)
  ]

-- | Respond to a request. This is where the behaviours of λGPT will go, but 
-- for now it just responds to "Hello".
-- Notice the new signature: it takes Memory, and returns IO Memory
respondTo :: [(String, String)] -> Request -> IO [(String, String)]

respondTo mem Hello = do
  putStrLn "Hi there!"
  pure mem -- Return the memory unchanged

respondTo mem Unknown = do
  putStrLn "I don't understand that."
  pure mem

respondTo mem WhatDay = do
  now <- getCurrentTime
  let today = utctDay now
  let dayStr = show (dayOfWeek today)
  putStrLn ("Today is " ++ dayStr ++ ".")
  pure mem

respondTo mem WhatDayTomorrow = do
  now <- getCurrentTime
  let today = utctDay now
  let tomorrow = addDays 1 today
  let dayStr = show (dayOfWeek tomorrow)
  putStrLn ("Tomorrow is " ++ dayStr ++ ".")
  pure mem

respondTo mem (HowLongAgo targetDate) = do
  now <- getCurrentTime
  let today = utctDay now
  let days = diffDays today targetDate
  putStrLn (show targetDate ++ " was " ++ show days ++ " days ago.")
  pure mem

respondTo mem (WhatIs expr) = do
  case evaluateExpr mem expr of
    Right result -> do
      putStrLn ("The answer is " ++ printLonghand result ++ ".")
      -- Save the result to our memory under the secret key "_that"
      pure (("_that", show result) : mem)
    Left errMsg -> do
      -- Print the error if they used "that" too early
      putStrLn errMsg
      pure mem

-- handling the memory
respondTo mem (Remember name thing) = do
  putStrLn "Okay."
  -- Add the new fact to the front of our memory list
  pure ((name, thing) : mem)

respondTo mem (TellMeAbout name) = do
  -- lookup: searches our [(String, String)] for the name
  case lookup name mem of
    Just thing -> putStrLn ("Sure - " ++ name ++ " is " ++ thing ++ ".")
    Nothing    -> putStrLn ("Sorry, I don't know anything about " ++ name ++ ".")
  pure mem




-- | Expression Parsing 

data Expr
  = Num Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | That
  deriving (Eq, Show)

evaluateExpr :: [(String, String)] -> Expr -> Either String Int
evaluateExpr _   (Num n)     = Right n
evaluateExpr mem That        =
  -- We'll use "_that" to store our last answer
  case lookup "_that" mem of
    Just val -> Right (read val) -- Convert the saved String back to an Int
    Nothing  -> Left "I haven't evaluated anything yet."

evaluateExpr mem (Add e1 e2) = do
  v1 <- evaluateExpr mem e1
  v2 <- evaluateExpr mem e2
  pure (v1 + v2)

evaluateExpr mem (Sub e1 e2) = do
  v1 <- evaluateExpr mem e1
  v2 <- evaluateExpr mem e2
  pure (v1 - v2)

evaluateExpr mem (Mul e1 e2) = do
  v1 <- evaluateExpr mem e1
  v2 <- evaluateExpr mem e2
  pure (v1 * v2)


parseExpr :: Parser Expr
parseExpr = do
  firstExpr <- parseNumOrThat
  operations <- many parseOpAndNum

  -- foldl for left-to-right
  pure ( foldl (\acc (opCtor, nextExpr) -> opCtor acc nextExpr) firstExpr operations )
  where
    -- A new helper that looks for "that" OR a longhand number
    parseNumOrThat :: Parser Expr
    parseNumOrThat = choice
      [ do
          _ <- string "that"
          pure That
      , do
          Num <$> parseLonghand
      ]

    parseOpAndNum :: Parser (Expr -> Expr -> Expr, Expr)
    parseOpAndNum = choice
      [ do
          _ <- string " plus "
          e <- parseNumOrThat
          pure (Add, e)
      , do
          _ <- string " minus "
          e <- parseNumOrThat
          pure (Sub, e)
      , do
          _ <- string " times "
          e <- parseNumOrThat
          pure (Mul, e)
      ]
