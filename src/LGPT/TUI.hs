module LGPT.TUI where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import LGPT.Helpers (Parser, prompt, runStart)
import LGPT.Numbers (parseLonghand, printLonghand)
import Data.Time (getCurrentTime, utctDay, dayOfWeek, addDays, diffDays, fromGregorian, Day)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime, localDay)
import Text.Megaparsec.Byte (string')
import LGPT.Weather (CurrentWeather(..), fetchWeather)
--------------------------------------------------------------------------------

-- | Represents the Syntax Tree for our mathematical expressions.
data Expr
  = Num Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | That
  deriving (Eq, Show)


data Request = Unknown | Hello | WhatDay | WhatDayTomorrow | HowLongAgo Day | WhatIs Expr|
              Remember String String | TellMeAbout String | WeatherToday | WeatherTomorrow
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Starts the main read => eval => print loop. 
-- I chose to pass the memory state [(String, String)] explicitly using recursion 
-- rather than using the "state" monad. This keeps the IO logic straight forward 
-- and avoids overcomplicating the simple loop structure.
runREPL :: IO ()
runREPL = loop [] -- Start the chatbot with an empty memory list
  where
    loop :: [(String, String)] -> IO ()
    loop memory = do
      putStr prompt
      req <- getLine
      newMemory <- respondTo memory (readRequest req)
      loop newMemory -- Loop again using the new memory

--------------------------------------------------------------------------------
-- Parsing and responding to requests:

-- | Reads a request from a String.
-- Runs the Megaparsec parser and converts any failed parses into an Unknown request.
readRequest :: String -> Request
readRequest str = case parse parseRequest "<stdin>" str of
  Left  err -> Unknown
  Right req -> req


-- | Parses user input into a Request data type.
-- The "try" combinator is used extensively here to allow backtracking. 
-- Because many requests share prefixes like "what is" and "what day is it", 
-- "try" prevents the parser from consuming input and failing permanently on a partial match.
parseRequest :: Parser Request
parseRequest = choice
  [ try $ do
      _ <- string' "hello" <|> string' "hi" <|> string' "hey"
      _ <- optional (char '.' <|> char '!' <|> char '?')
      _ <- space
      pure Hello

  , try $ do
      _ <- string' "What is the weather like today?" <|> string "What is the weather like today"
      pure WeatherToday

  , try $ do
      _ <- string' "what is the weather like today"
      _ <- optional (char '?')
      _ <- space
      pure WeatherToday

  , try $ do
      _ <- string' "what day is it tomorrow" 
      _ <- optional (char '?')
      _ <- space
      pure WhatDayTomorrow

  , try $ do
      _ <- string' "what day is it"
      _ <- optional (char '?')
      _ <- space
      pure WhatDay

  , try $ do
        _ <- string' "how long ago was "
        year <- some digitChar
        _ <- char '-'
        month <- some digitChar
        _ <- char '-'
        day <- some digitChar
        _ <- optional (char '?')
        _ <- space
        let targetDate = fromGregorian (read year) (read month) (read day)
        pure (HowLongAgo targetDate)

  , try $ do
      _ <- string' "what is "
      expr <- parseExpr
      _ <- optional (char '?')
      _ <- space
        pure (WhatIs expr)

  , try $ do
      _ <- string' "remember that " 
      name <- someTill anySingle (string' " is ") 
      thing <- someTill anySingle (optional (char '.') >> space >> eof) -- Reads until the end of the line
      pure (Remember name thing)

  , do
      _ <- string' "Tell me about " -- grab the name, stopping if they typed a period at the end
      name <- someTill anySingle (char '.') <|> some anySingle
      pure (TellMeAbout name)
  ]


-- | Parses mathematical expressions from English words.
-- I chose to use "foldl" to bundle the arithmetic operations together. 
-- This purposefully bypasses standard BODMAS precedence, which enforces a strict 
-- left-to-right evaluation order to mimic how natural language is read aloud.
parseExpr :: Parser Expr
parseExpr = do
  firstExpr <- parseNumOrThat
  operations <- many parseOpAndNum
  -- foldl for left-to-right
  pure ( foldl (\acc (opCtor, nextExpr) -> opCtor acc nextExpr) firstExpr operations )
  where
    -- A helper that looks for "that" or a longhand number
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


-- | Pattern matches on the parsed Request, executes the appropriate IO actions 
-- (like fetching the time or calling an API), and returns the updated memory state.
respondTo :: [(String, String)] -> Request -> IO [(String, String)]

respondTo mem Hello = do
  putStrLn "Hi there!"
  pure mem -- Return the memory unchanged

respondTo mem Unknown = do
  putStrLn "I don't understand that."
  pure mem

respondTo mem WhatDay = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone              -- Get the local timezone
  let localTime = utcToLocalTime tz now -- Convert UTC 'now' to local time
  let today = localDay localTime        -- Extract the date from the local time
  let dayStr = show (dayOfWeek today)
  putStrLn ("Today is " ++ dayStr ++ ".")
  pure mem

respondTo mem WhatDayTomorrow = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  let localTime = utcToLocalTime tz now
  let today = localDay localTime
  let tomorrow = addDays 1 today        -- Add 1 day to the LOCAL today
  let dayStr = show (dayOfWeek tomorrow)
  putStrLn ("Tomorrow is " ++ dayStr ++ ".")
  pure mem

respondTo mem (HowLongAgo targetDate) = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  let localTime = utcToLocalTime tz now
  let today = localDay localTime        -- Use local today for the calculation
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
  case lookup name mem of
    Just thing -> putStrLn ("Sure - " ++ name ++ " is " ++ thing ++ ".")
    Nothing    -> putStrLn ("Sorry, I don't know anything about " ++ name ++ ".")
  pure mem

respondTo mem WeatherToday = do
  weatherData <- fetchWeather "current"
  case weatherData of
    Just w -> do
      putStrLn ("The weather is " ++ currDesc w ++ " and it is " ++ show (currTemp w) ++ " degrees.")
    Nothing -> putStrLn "Sorry, I couldn't reach the weather service right now."
  pure mem

respondTo mem WeatherTomorrow = do
  -- Fetch the forecast endpoint for tomorrow's weather
  weatherData <- fetchWeather "forecast" 
  case weatherData of
    Just w -> do
      putStrLn ("Tomorrow the weather is " ++ currDesc w ++ " and it is " ++ show (currTemp w) ++ " degrees.")
    Nothing -> putStrLn "Sorry, I couldn't get the forecast."
  pure mem


-- | Evaluates the parsed expression.
-- I opted to return an "Either String Int" instead of a plain "Int". 
-- This allows the program to catch and report errors (like the user 
-- referencing "That" before any math has been evaluated) without crashing the chatbot.
evaluateExpr :: [(String, String)] -> Expr -> Either String Int
evaluateExpr _   (Num n)     = Right n
evaluateExpr mem That        =
  -- We'll use "_that" to store our last answer
  case lookup "_that" mem of
    Just val -> Right (read val) -- convert the saved str back to an int
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
--------------------------------------------------------------------------------