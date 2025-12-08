module Day07 (run) where

import Data.List (tails)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

type BoolVec = Vector Bool
type IntVec = Vector Int

run = do
  txt <- readFile "data/day07.txt"
  let lns = lines txt

  let startLine :: IntVec
      startLine = V.fromList $ map (\c -> if c == 'S' then 1 else 0) (head lns)
  let m = map (V.fromList . map (== '^')) (tail lns)

  let (res, count) = foldl propagate (startLine, 0) m

  putStrLn ("Part 1: " ++ show count)
  putStrLn ("Part 2: " ++ show (sum res))

propagate :: (IntVec, Int) -> BoolVec -> (IntVec, Int)
propagate (beams, splitCount) splitters =
  let empty = V.fromList $ replicate len 0
      (next, splitCount') =
        foldl go (empty, splitCount) [0 .. length beams - 1]
   in (next, splitCount')
  where
    len = length beams
    go :: (IntVec, Int) -> Int -> (IntVec, Int)
    go (newBeams, splitCount) i
      | splitter && beam > 0 =
          let updates = [(i - 1, beam), (i + 1, beam)]
              updates' = filter (updateInBounds len) updates
           in (newBeams +// updates', splitCount + 1)
      | not splitter && beam > 0 = (newBeams +// [(i, beam)], splitCount)
      | otherwise = (newBeams, splitCount)
      where
        splitter = splitters ! i
        beam = beams ! i
        updateInBounds len (idx, _) = idx >= 0 && idx < len

-- Utils
(+//) :: Num a => V.Vector a -> [(Int, a)] -> V.Vector a
v +// ups = V.accum (+) v ups

windowPadded :: Int -> a -> [a] -> [[a]]
windowPadded n pad xs =
  let left = (n - 1) `div` 2
      right = (n - 1) - left
      padl = replicate left pad
      padr = replicate right pad
   in windows n (padl ++ xs ++ padr)

windows :: Int -> [a] -> [[a]]
windows n xs = map (take n) $ takeWhile ((>= n) . length) (tails xs)
