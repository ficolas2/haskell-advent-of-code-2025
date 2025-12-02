module Day01 (run) where

import Data.List (foldl')

run :: IO ()
run = do
  txt <- readFile "data/day01.txt"
  let values = map parse $ lines txt
  print $ foldl step (0, 0, 50) values

step :: (Int, Int, Int) -> Int -> (Int, Int, Int)
step (n1, n2, start) delta =
  let modVal = (start + delta) `mod` 100

      n1' = if modVal == 0 then n1 + 1 else n1

      passes
        | delta > 0 = (start + delta) `div` 100
        | delta < 0 =
            let start' = if start == 0 then 100 else start
             in abs ((start' + delta - 1) `div` 100)
        | otherwise = 0
   in (n1', n2 + passes, modVal)

parse :: String -> Int
parse ('L' : ns) = -read ns
parse ('R' : ns) = read ns
parse _ = error "Invalid input"
