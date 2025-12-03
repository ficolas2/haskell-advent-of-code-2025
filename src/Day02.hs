module Day02 (run) where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

run = do
  txt <- readFile "data/day02.txt"
  let line = head (lines txt)
  let ids = expandRanges $ map parse (splitOn "," line)

  print $ sum $ filter isFake1 ids
  print $ sum $ filter isFake2 ids

parse :: String -> (Int, Int)
parse str =
  let [lo, hi] = splitOn "-" str
   in (read lo, read hi)

-- Part 1
isFake1 :: Int -> Bool
isFake1 n
  | Just (a, b) <- twoHalves n = a == b
  | otherwise = False

expandRanges :: [(Int, Int)] -> [Int]
expandRanges = concatMap (\(a, b) -> [a .. b])

twoHalves :: Int -> Maybe (String, String)
twoHalves n
  | n < 10 = Nothing
  | otherwise =
      let s = show n
          (a, b) = splitAt (length s `div` 2) s
       in Just (a, b)

-- Part 2
isFake2 :: Int -> Bool
isFake2 n = isFake2' (show n) 1

isFake2' :: String -> Int -> Bool
isFake2' str l
  | l >= len = False
  | len `mod` l == 0 && repeated == str = True
  | otherwise = isFake2' str (l + 1)
  where
    len = length str
    base = take l str
    reps = length str `div` l
    repeated = concat $ replicate reps base
