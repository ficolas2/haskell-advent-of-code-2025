module Day05 (run) where

import Data.Ix (Ix (range))
import Data.List (mapAccumL, sortOn)
import Data.Maybe (catMaybes)

data Range = Range Int Int

instance Show Range where
  show (Range a b) = show a ++ "-" ++ show b

run = do
  txt <- readFile "data/day05.txt"
  let (freshRangesStr, "" : ingredientsStr) = break (== "") (lines txt)

  let ingredients :: [Int]
      ingredients = map read ingredientsStr
  let ranges = map readRange freshRangesStr

  -- Part 1
  let freshIngredients = map (\i -> any (`inRange` i) ranges) ingredients
  let freshCount = length $ filter id freshIngredients
  putStrLn $ "Fresh ingredients: " ++ show freshCount

  -- Part 2
  let combinedRanges = combineRanges ranges
  let possibleFreshCount = sum $ map rangeDomain combinedRanges
  putStrLn $ "Possible fresh ingredients: " ++ show possibleFreshCount

combineRanges :: [Range] -> [Range]
combineRanges ranges = foldl go [] (sortOn (\(Range l _) -> l) ranges)
  where
    go :: [Range] -> Range -> [Range]
    go [] r = [r]
    go (x : xs) r
      | Just nr <- mergeRange x r = nr : xs
      | otherwise = r : x : xs

readRange :: String -> Range
readRange rangeStr
  | (start, '-' : end) <- break (== '-') rangeStr = Range (read start) (read end)

inRange :: Range -> Int -> Bool
inRange (Range low high) n = n >= low && n <= high

rangeDomain :: Range -> Int
rangeDomain (Range low high) = (high - low) + 1

rangeOverlap :: Range -> Range -> Bool
rangeOverlap (Range low1 high1) (Range low2 high2) =
  low1 <= high2 && low2 <= high1

mergeRange :: Range -> Range -> Maybe Range
mergeRange r1@(Range low1 high1) r2@(Range low2 high2)
  | rangeOverlap r1 r2 =
      Just $ Range (min low1 low2) (max high1 high2)
  | otherwise = Nothing
