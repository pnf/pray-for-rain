-- {-# LANGUAGE OverloadedStrings #-}  This will mess up regex

module Pray.Regex (zipRange) where

import  Text.Regex.Posix
import qualified  Data.Vector as V

zipRange :: String -> V.Vector String
zipRange cell = 
  let zss = cell =~ "[1-9][0-9]{4}" :: [[String]]
      zsn = fmap (read . head) zss :: [Int]
  in  case zsn of
        [a] -> V.fromList $ [show a]
        [a,b] -> V.fromList $ fmap show [a..b]
        _ -> V.empty

