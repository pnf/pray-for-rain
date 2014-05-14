module Pray.Main (main) where

import Pray.Zips
import Pray.Forecast
import qualified  Data.Vector as V
import qualified System.Random as R


main = do
  zips <- readZips
  let n = V.length zips
  i <- R.randomRIO (0 :: Int, n-1)
  let zip = zips V.! i
  fc <- getForecast zip
  loc <- location zip
  let mytown = town loc
  let (rain,sum) = summarize fc
  let txt = "They're saying " ++ sum ++ " in " ++ mytown ++ ". Please pray for " ++ (if rain then "sun." else "rain.")
  print txt
    
