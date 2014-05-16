{-# LANGUAGE OverloadedStrings,
              DeriveDataTypeable,
              DeriveGeneric,
              FlexibleContexts,
              TemplateHaskell,
              RankNTypes,
              PatternGuards #-}

module Pray.Forecast (getForecast,summarize) where

import Control.Monad.Trans.Either
import Control.Exception
import Network.HTTP.Conduit -- the main module
import GHC.Generics
import           Data.Aeson
import           Control.Applicative
import           Control.Monad
import qualified Data.Text as T
import qualified Data.Char as C
--import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 (ByteString)
-- import qualified Data.ByteString.Lazy.Internal.ByteString as DBLIB

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

-- The EitherT transformers seem to expect that the left clause doesnt change type
eitherSimpleHttp :: String -> IO (Either String ByteString)
eitherSimpleHttp url = do
  eitherResp <- try $ simpleHttp url :: IO (Either SomeException ByteString)
  return $ case eitherResp of
    Left e -> Left $ show e
    Right b -> Right b

getForecast :: String -> String -> IO (Either String String)
getForecast apikey zip = runEitherT $ do
  let url = "http://api.wunderground.com/api/" ++ apikey ++ "/forecast/q/NY/" ++ zip ++ ".json"
  resp <- EitherT $ eitherSimpleHttp url
  wur <- hoistEither $ eitherDecode resp
  return $ extractIcon wur

getForecast2 :: String -> String -> IO (Either String String)
getForecast2 apikey zip = runEitherT $ do
  let url = "http://api.wunderground.com/api/" ++ apikey ++ "/forecast/q/NY/" ++ zip ++ ".json"
  let respe = EitherT $ try $ simpleHttp url :: EitherT (SomeException) (IO) ByteString
  resp <- bimapEitherT show id $ respe
  wur <- hoistEither $ eitherDecode resp
  return $ extractIcon wur

summarize :: String -> (Bool,String)
summarize fc =
  let s1 = C.toLower <$> (fc =~ ("(^[^\\.]+)"  :: String) :: String)
      rain = s1 =~ ("thunderstorm|rain|shower|cloud|overcast" :: String) :: Bool
  in
      (rain,s1)
  
