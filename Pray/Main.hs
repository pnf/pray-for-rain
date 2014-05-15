module Pray.Main (main) where

import Pray.Zips
import Pray.Forecast
import Pray.Twilio
import qualified  Data.Vector as V
import qualified System.Random as R
import Control.Exception
import Control.Concurrent (threadDelay)

data Config = Config {
  zips :: (V.Vector String),
  wuApiKey :: String,
  twilioAcct :: String,
  twilioApiKey :: String
  } deriving (Show,Read)


prayFor :: Config -> String -> IO ()
prayFor config zip = do
      fc <- getForecast (wuApiKey config) zip
      loc <- location zip
      let mytown = town loc
      let (rain,sum) = summarize fc
      let txt = "They're saying " ++ sum ++ " in " ++ mytown ++ ". Please pray for " ++ (if rain then "sun." else "rain.")
      print txt


getConfig :: IO Config
getConfig = do
  zips <- readZips
  wuApiKey <- readFile "WUAPI"
  tConf <- readFile "TWILIO"
  let _:tAcct:tApiKey:_ = lines tConf
  return $ Config zips wuApiKey tAcct tApiKey

main = do
  config <- getConfig
  loop config 5 where
    loop :: Config -> Int -> IO ()
    loop _ 0 = do {print "Done!"}
    loop config j = do
      let n = V.length (zips config)
      i <- R.randomRIO (0 :: Int, n-1)
      let zip = (zips config) V.! i
      foo <-  (try $ prayFor config zip) :: IO (Either SomeException ())
      print foo

      threadDelay 5000000
      loop config (j-1)
