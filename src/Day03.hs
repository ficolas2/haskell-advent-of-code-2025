module Day03 (run) where

import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

run = do
  txt <- readFile "data/day03.txt"
  let banks = map (map digitToInt) (lines txt)
  print $ sum $ map (getMaxPower 2) banks
  print $ sum $ map (getMaxPower 12) banks

getMaxPower :: Int -> [Int] -> Int
getMaxPower 0     _  = 0
getMaxPower count ns =
  let max = maximum $ butLastN (count - 1) ns
      index = fromJust $ elemIndex max ns
      subList = drop(index + 1) ns
    in max * 10 ^ (count - 1) + getMaxPower (count - 1) subList

butLastN :: Int -> [a] -> [a]
butLastN n xs = take (length xs - n) xs
