module LGPT.TUI where

{-
This file is the main entry point to your coursework.

You can create or modify any files in src/ as much as you like. The 
code that is included here is a good starting point, but you don't need to 
keep it if you don't want to.
-}
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import LGPT.Helpers (Parser, prompt, runStart)   
import LGPT.Numbers (parseLonghand, printLonghand)
import Data.Time (getCurrentTime, utctDay, dayOfWeek, addDays, diffDays, fromGregorian, Day)

--------------------------------------------------------------------------------
{- | Our program. It runs a loop which:
      1. Reads a line of input
      2. Parses it into a structured Request
      3. Does something based on that request (normally printing something out).
-}
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
  let result = evaluateExpr expr
  putStrLn ("The answer is " ++ printLonghand result ++ ".")
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
  deriving (Eq, Show)

evaluateExpr :: Expr -> Int
evaluateExpr (Num n)     = n
evaluateExpr (Add e1 e2) = evaluateExpr e1 + evaluateExpr e2
evaluateExpr (Sub e1 e2) = evaluateExpr e1 - evaluateExpr e2
evaluateExpr (Mul e1 e2) = evaluateExpr e1 * evaluateExpr e2

parseExpr :: Parser Expr
parseExpr = do
  firstNum <- Num <$> parseLonghand -- grab the very first number using the longhand parser provided 
  operations <- many parseOpAndNum -- grab zero or more (operator, next number) pairs
  
  -- foldl for left-to-right
  pure ( foldl (\acc (opCtor, nextNum) -> opCtor acc (Num nextNum)) firstNum operations )
  where
    parseOpAndNum :: Parser (Expr -> Expr -> Expr, Int)
    parseOpAndNum = choice
      [ do 
          _ <- string " plus "
          n <- parseLonghand
          pure (Add, n)
      , do 
          _ <- string " minus "
          n <- parseLonghand
          pure (Sub, n)
      , do 
          _ <- string " times "
          n <- parseLonghand
          pure (Mul, n)
      ]

