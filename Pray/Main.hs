module Pray.Main (main) where

import Pray.Zips as Z
import Pray.Forecast
import qualified Pray.Twilio as T
import qualified  Data.Vector as V
import qualified System.Random as R
import Control.Exception
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import System.Environment

data Config = Config {
  zips :: (V.Vector String),
  wuApiKey :: String,
  twilioAcct :: String,
  twilioApiKey :: String,
  twilioAgent :: T.Agent,
  phoneNumber :: String,
  maxIter :: Int,
  periodSec :: Int
  }


getConfig :: [String] -> IO Config
getConfig args = do
  zips <- readZips
  wuApiKey <- readFile "WUAPI"
  tConf <- readFile "TWILIO"
  let _:tAcct:tApiKey:_ = lines tConf
  tAgent <- T.twilioAgent
  let phoneNumber:maxIterS:periodS:_ = args
  return $ Config zips wuApiKey tAcct tApiKey tAgent phoneNumber (read maxIterS) (read periodS)


prayFor :: Config -> String -> IO (Either String String)
prayFor config zip = runEitherT $ do
      fc <-  EitherT $ getForecast (wuApiKey config) zip
      loc <- EitherT $ location zip
      mytown <- hoistEither $ town loc
      let (rain,sum) = summarize fc
      let txt = "They're saying " ++ sum ++ " in " ++ mytown ++ ". Please pray for " ++
                (if rain then "sun." else "rain.")
      return txt

main = do
  args <- getArgs
  config <- getConfig args
  loop config (maxIter config) where
    loop :: Config -> Int -> IO ()
    loop _ 0 = do {print "Done!"}
    loop config j = do
      let n = V.length (zips config)
      i <- R.randomRIO (0 :: Int, n-1)
      let zip = (zips config) V.! i
      res <- runEitherT $ do
         prayer <- EitherT $  prayFor config zip
         sms <- EitherT $ T.sendMessage (twilioAgent config) (phoneNumber config) ("wwpray " ++ prayer)
         return $ "Sent: " ++ prayer
      case res of
        Left e -> print $ "Ignoring exception: " ++ show e
        Right m -> print m

      threadDelay $ (periodSec config)*1000*1000
      loop config (j-1)
