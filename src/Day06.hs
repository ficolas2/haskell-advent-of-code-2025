module Day06 (run) where

import Data.List (transpose)

run = do
  txt <- readFile "data/day06.txt"

  let rows = map words (lines txt)

  let ops = map mapOps (last rows)
  let numbers :: [[Int]]
      numbers = transpose $ map (map read) (init rows)
  putStrLn $ "Part 1: " ++ show (sum $ zipWith foldl1 ops numbers)

  let numbersT = transpose $ init $ lines txt
  let numbersTSplit :: [[Int]]
      numbersTSplit = map (map read) (splitBy (all (== ' ')) numbersT)
  putStrLn $ "Part 2: " ++ show (sum $ zipWith foldl1 ops numbersTSplit)

mapOps :: String -> (Int -> Int -> Int)
mapOps "+" = (+)
mapOps "-" = (-)
mapOps "*" = (*)

splitBy :: ([a] -> Bool) -> [[a]] -> [[[a]]]
splitBy _ [] = []
splitBy pred xs =
  let (first, rest) = break pred xs
      rest' = case rest of
        [] -> []
        (_ : rs) -> rs
   in first : splitBy pred rest'
