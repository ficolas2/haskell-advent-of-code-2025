module Day04 (run) where

type Map = [[Bool]]

run = do
  txt <- readFile "data/day04.txt"
  let boolMap = map (map (== '@')) (lines txt)
  let m = boolMap
  let accesible = getAccessibleMap m
  putStrLn $ "Accesible at start: " ++ show (countTrues2D accesible)
  putStrLn $ "Total acesible: " ++ show (getTotalAccesible m)

getTotalAccesible m =
  let accesible = getAccessibleMap m
      count = countTrues2D accesible
      newMap = removeAccesibles m accesible
   in if count == 0 then count else count + getTotalAccesible newMap

removeAccesibles :: Map -> Map -> Map
removeAccesibles = zipWith (zipWith removeIfAccesible)
  where
    removeIfAccesible a b = a && not b

getAccessibleMap m =
  let indices = indices2D (length m) (length (head m))
   in map (map (canAccess m)) indices

canAccess :: Map -> (Int, Int) -> Bool
canAccess m pos@(x, y) =
  let positions = map (addPos pos) tilesAround
      tilesWithPaper = length $ filter id $ map (uncurry (hasPaper m)) positions
      hasRoll = m !! y !! x
   in hasRoll && tilesWithPaper < 4

hasPaper :: Map -> Int -> Int -> Bool
hasPaper m x y
  | y < 0 || y >= length m = False
  | x < 0 || x >= length (head m) = False
  | otherwise = m !! y !! x

-- Utils
addPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

indices2D :: Int -> Int -> [[(Int, Int)]]
indices2D cols rows = [[(j, i) | j <- [0 .. cols - 1]] | i <- [0 .. rows - 1]]

tilesAround :: [(Int, Int)]
tilesAround = do
  x <- [-1 .. 1]
  y <- [-1 .. 1]
  if (x, y) /= (0, 0) then return (x, y) else []

countTrues2D = length . concatMap (filter id)

mapToString :: [[Bool]] -> String
mapToString m = unlines $ map (map toChar) m
  where
    toChar True = '@'
    toChar False = '.'
