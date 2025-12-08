module Day08 (run) where

import Data.Foldable (foldl')
import Data.List (nub, sortOn, tails)
import Data.List.Split (splitOn)
import qualified Data.Ord
import Data.Set (Set)
import qualified Data.Set as S

joinCount = 1000

circuitCount = 3

type PosSet = Set Position

run = do
  txt <- readFile "data/day08.txt"
  let posList = map parsePosition (lines txt)
  let pairs = [(x, y) | (x : ys) <- tails posList, y <- ys]
  let sortedPairs = sortOn (uncurry distance2) pairs

  let joinedPairs = take joinCount sortedPairs

  -- Part 1
  let sortedCircuits = sortOn (Data.Ord.Down . length) (foldl' mergePairs [] joinedPairs)
  putStrLn $ "Part 1: " ++ show (product $ map length $ take circuitCount sortedCircuits)

  -- Part 2
  let (Position x1 _ _, Position x2 _ _) = part2 (length posList) sortedPairs []
  putStrLn $ "Part 1: " ++ show (x1 * x2)

  where
    part2 :: Int -> [(Position, Position)] -> [PosSet] -> (Position, Position)
    part2 count (pair : rest) sets =
      let newSets = mergePairs sets pair
       in if length newSets == 1 && (length (head newSets) == count)
            then
              pair
            else part2 count rest newSets

mergePairs :: [PosSet] -> (Position, Position) -> [PosSet]
mergePairs sets (a, b) =
  let (hits, rest) =
        foldl' classify ([], []) sets
      merged = S.unions (S.fromList [a, b] : hits)
   in merged : rest
  where
    classify (hs, rs) s
      | a `S.member` s || b `S.member` s = (s : hs, rs)
      | otherwise = (hs, s : rs)

parsePosition :: String -> Position
parsePosition str
  | [x, y, z] <- splitOn "," str = Position (read x) (read y) (read z)
  | otherwise = error ("Error parsing position " ++ str)

-- Utils
data Position = Position Int Int Int

instance Eq Position where
  (Position x1 y1 z1) == (Position x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2

instance Ord Position where
  compare (Position x1 y1 z1) (Position x2 y2 z2) =
    compare (x1, y1, z1) (x2, y2, z2)

instance Show Position where
  show (Position x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

distance2 :: Position -> Position -> Int
distance2 (Position x1 y1 z1) (Position x2 y2 z2) =
  (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

getX :: Position -> Int
getX (Position x _ _) = x
