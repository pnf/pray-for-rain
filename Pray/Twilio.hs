{-# LANGUAGE OverloadedStrings,
              DeriveDataTypeable,
              DeriveGeneric,
              FlexibleContexts,
              TemplateHaskell,
              RankNTypes,
              PatternGuards #-}

module Pray.Twilio (sendMessage, getMessages) where

import Network.HTTP.Conduit -- the main module
--import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Conduit

import GHC.Generics
import           Data.Aeson
import           Control.Applicative
import           Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Char as C
import Data.Maybe
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack,unpack)



data Agent = Agent {
  manager :: Manager,
  smsReq :: Request,
  fromNum :: ByteString,
  acct :: ByteString,
  auth :: ByteString
  }

twilioAgent :: IO Agent
twilioAgent = do
  tConf <- readFile "TWILIO"
  let num:acct:auth:_= pack `fmap` (lines tConf)
  let baseUrl = "https://api.twilio.com/2010-04-01/Accounts/"
  urlReq <- parseUrl $ baseUrl ++ unpack acct ++ "/Messages.json"
  let req = applyBasicAuth acct auth urlReq
  print (urlReq,req,num,acct,auth)
  manager <- newManager tlsManagerSettings
  return $ Agent manager req num acct auth


getMessages :: String
getMessages = "bye"

toBS :: [(String,String)] -> [(ByteString,ByteString)]
toBS = fmap $ \ (s1,s2) -> (pack s1, pack s2)

sendMessage :: Agent -> String -> String -> IO ()
sendMessage agent to msg = do
  let req = urlEncodedBody  [("To",pack to),
                             ("From", (fromNum agent)),
                             ("Body", pack msg)]  (smsReq agent)
  print req
  withManager $ \m -> (print . responseBody) <$> httpLbs req m
  print "ha"
  
--  res <- httpLbs req (manager agent)
--  print res

-- QUM0ODk3YjE4OWQxYjBjNzBmNTRjNDBiYjhmMmRjYWU3NDpmNDcyMzFhMDM1MzVjYzYyODQ0MGQwODYwMGJjMWRiNQ==
-- QUM0ODk3YjE4OWQxYjBjNzBmNTRjNDBiYjhmMmRjYWU3NDpmNDcyMzFhMDM1MzVjYzYyODQ0MGQwODYwMGJjMWQ1Yg==
