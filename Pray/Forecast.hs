{-# LANGUAGE OverloadedStrings,
              DeriveDataTypeable,
              DeriveGeneric,
              FlexibleContexts,
              TemplateHaskell,
              RankNTypes,
              PatternGuards #-}

module Pray.Forecast (getForecast,summarize) where

import Network.HTTP.Conduit -- the main module
import GHC.Generics
import           Data.Aeson
import           Control.Applicative
import           Control.Monad
import qualified Data.Text as T
import qualified Data.Char as C

import  Text.Regex.Posix


data WUResponse = WUResponse  { 
      forecast :: Forecast
    } deriving (Eq,Show,Generic)

data Forecast = Forecast {
      textForecast :: TextForecast
    } deriving (Eq, Show, Generic)

data TextForecast = TextForecast {
      date ::T.Text,
      forecastDays :: [ForecastDay]
    } deriving (Eq, Show, Generic)
      
data ForecastDay = ForecastDay {
      period :: Int,
      icon :: T.Text,
      fcttext :: T.Text
    } deriving (Eq,Show,Generic)


-- "{\"forecast\" : {\"txt_forecast\" :{\"date\" : \"cegodnya\", \"forecastday\" : [{\"period\" : 0, \"icon\" : \"sun\"}]}}}"

-- (.:) :: FromJSON a => Object -> T.Text -> Parser a

instance FromJSON WUResponse where
    -- parseJSON :: FromJSON a => Value -> Parser a
    parseJSON (Object o) = do
      forecast <- parseJSON =<< (o .: "forecast")
      return $ WUResponse forecast
    parseJSON _ = mzero

-- "{\"forecastday\" : [{\"period\" : 0, \"icon\" : \"sun\"}]}"
instance FromJSON Forecast where -- Generic trick doesn't work here
    parseJSON (Object o) = do
      textForecast <- parseJSON =<< (o .: "txt_forecast")
      return $ Forecast textForecast
    parseJSON _ = mzero

instance FromJSON TextForecast where
    parseJSON (Object o) = do
      date <- (o .: "date")
      forecastDays <- mapM parseJSON =<< (o .: "forecastday")
      return $ TextForecast date forecastDays
    parseJSON _ = mzero
      

-- Simple JSON record can be built from generics
instance FromJSON ForecastDay  --where
--    parseJSON (Object o) = ForecastDay <$> o .: "period" <*> o .: "icon"
--    parseJSON _ = mzero      
-- simpleHttp  :: Control.Monad.IO.Class.MonadIO m => String -> m C.ByteString

extractIcon :: WUResponse -> String
extractIcon wur = T.unpack $ fcttext $ (!! 1)  $ forecastDays  $ textForecast $ forecast wur

getForecast :: String -> IO String
getForecast zip = do
  let url = "http://api.wunderground.com/api/fdbe061bc411ad6b/forecast/q/NY/" ++ zip ++ ".json"
  d <- (eitherDecode <$> simpleHttp url) :: IO (Either String WUResponse)
  case d of
    Left err -> return $ err
    Right wur -> return $ extractIcon $  wur

summarize :: String -> (Bool,String)
summarize fc =
  let s1 = C.toLower <$> (fc =~ ("(^[^\\.]+)"  :: String) :: String)
      rain = s1 =~ ("thunderstorm|rain|shower|cloud|overcast" :: String) :: Bool
  in
      (rain,s1)

  
  
