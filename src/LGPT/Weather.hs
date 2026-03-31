module LGPT.Weather 
  ( CurrentWeather(..)
  , fetchWeather 
  ) where

import Data.Aeson (FromJSON(..), (.:), withObject)
import qualified Data.Aeson.Key as K
import Network.HTTP.Simple (httpJSONEither, getResponseBody, parseRequest_)

-- | The type your chatbot expects
data CurrentWeather = CurrentWeather 
  { currDesc :: String
  , currTemp :: Int 
  } deriving (Show)

-- | Temporary types to catch Open-Meteo's specific JSON shapes
data OMCurrent = OMCurrent Float Int
data OMDaily   = OMDaily [Float] [Int]

-- | Parse the "current" weather JSON
instance FromJSON OMCurrent where
  parseJSON = withObject "OMCurrent" $ \v -> do
    current <- v .: K.fromString "current"
    t <- current .: K.fromString "temperature_2m"
    c <- current .: K.fromString "weather_code"
    return (OMCurrent t c)

-- | Parse the "daily" forecast JSON
instance FromJSON OMDaily where
  parseJSON = withObject "OMDaily" $ \v -> do
    daily <- v .: K.fromString "daily"
    t <- daily .: K.fromString "temperature_2m_max"
    c <- daily .: K.fromString "weather_code"
    return (OMDaily t c)

-- | Translate Open-Meteo's WMO numeric codes into English words
wmoToDesc :: Int -> String
wmoToDesc 0 = "clear"
wmoToDesc 1 = "mainly clear"
wmoToDesc 2 = "partly cloudy"
wmoToDesc 3 = "overcast"
wmoToDesc 45 = "foggy"
wmoToDesc 48 = "foggy"
wmoToDesc 51 = "drizzling"
wmoToDesc 61 = "raining"
wmoToDesc 71 = "snowing"
wmoToDesc 95 = "stormy"
wmoToDesc _  = "variable"

-- | Fetch the weather (No API key needed!)
fetchWeather :: String -> IO (Maybe CurrentWeather)
fetchWeather "current" = do
  let url = "https://api.open-meteo.com/v1/forecast?latitude=52.4814&longitude=-1.8998&current=temperature_2m,weather_code"
  let req = parseRequest_ url
  
  response <- httpJSONEither req
  case getResponseBody response of
    Left _err -> pure Nothing
    Right (OMCurrent temp code) -> 
      pure (Just (CurrentWeather (wmoToDesc code) (round temp)))

fetchWeather "forecast" = do
  let url = "https://api.open-meteo.com/v1/forecast?latitude=52.4814&longitude=-1.8998&daily=temperature_2m_max,weather_code&timezone=Europe%2FLondon"
  let req = parseRequest_ url
  
  response <- httpJSONEither req
  case getResponseBody response of
    Left _err -> pure Nothing
    Right (OMDaily temps codes) -> 
      if length temps > 1 && length codes > 1
      then pure (Just (CurrentWeather (wmoToDesc (codes !! 1)) (round (temps !! 1))))
      else pure Nothing

fetchWeather _ = pure Nothing