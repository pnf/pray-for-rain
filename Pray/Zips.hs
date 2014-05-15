{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Pray.Zips (getZips,location,readZips,town) where

import qualified Data.Text as T
import  Text.Regex.Posix
import qualified  Data.Vector as V
import Network.HTTP.Conduit -- the main module
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, content, element, fromDocument, child,
                        ($//), (&|), (&/), (&//), (>=>))

import qualified System.Random as R
import Text.Printf

import Data.Aeson
import Control.Applicative
import Control.Monad


data ZipLoc = ZipLoc {zip :: String, places :: [Place]} deriving (Eq,Show)
data Place =  Place  {city :: String, state :: String} deriving (Eq,Show)

instance FromJSON ZipLoc where
  parseJSON (Object o) = ZipLoc
                         <$> o .: "post code"
                         <*> o .: "places"
  parseJSON _ = mzero

instance FromJSON Place where
  parseJSON (Object o) = Place
                         <$> o .: "place name"
                         <*> o .: "state"


-- The explicit :: String seems to be necessary when OverloadedStrings is set
-- "12345" -> 12345;  "12345 thru 12349" -> [12345,12346,12347,12348,12349]
zipRange :: String -> V.Vector String
zipRange cell = if cell == "\160" then V.empty else
  let zss = (cell :: String) =~  ("[1-9][0-9]{4}" :: String) :: [[String]]
      zsn = fmap (read . head) zss :: [Int]
  in  case zsn of
        [a] -> V.fromList $ [show a]
        [a,b] -> V.fromList $ fmap show [a..b]
        _ -> V.empty

-- Get valid zip codes from a friendly purveyor
getZips :: IO (V.Vector String)
getZips = do
  let url = "http://www.phaster.com/zip_code.html"
  html <- simpleHttp url
  let cursor = fromDocument . parseLBS $ html
  let rows = tail $ cursor $// element "table" &/ element "tr"
      
  let zstrs = concatMap (\row -> (row $// element "td") !! 2 $// content) rows
  let zvec = foldl1 (V.++) $ fmap (zipRange . T.unpack) zstrs
  return zvec

writeZips :: IO ()
writeZips = do
  let file = "ZIPCODES"
  zips <- getZips
  writeFile file $ show zips

readZips :: IO (V.Vector String)
readZips = do
  let file = "ZIPCODES"
  zstr <- readFile file
  return (read zstr)

location :: String -> IO (Either String ZipLoc)
location zip = do
  let uri = "http://api.zippopotam.us/us/" ++ zip
  resp <- simpleHttp uri
  eitherDecode <$> simpleHttp uri

-- Find a random town
town :: (Either String ZipLoc) -> String
town loc = case loc of
  Right (ZipLoc _ ((Place c s):_)) -> c ++ ", " ++ s
  _ -> "My town"




-- &| (!! 2) $// content
