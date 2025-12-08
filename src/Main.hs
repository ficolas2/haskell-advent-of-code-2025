module Main where

import System.Environment (getArgs)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08

main :: IO ()
main = do
  args <- getArgs
  case args of
    [nStr] ->
      case reads nStr of
        [(n, "")] -> runTask n
        _ -> putStrLn "Invalid day number"
    _ -> putStrLn "Usage: prog <day>"

runTask :: Int -> IO ()
runTask 1 = Day01.run
runTask 2 = Day02.run
runTask 3 = Day03.run
runTask 4 = Day04.run
runTask 5 = Day05.run
runTask 6 = Day06.run
runTask 7 = Day07.run
runTask 8 = Day08.run
runTask n
  | n > 0 && n <= 12 = putStrLn "Day not done yet"
  | otherwise        = putStrLn "Number must be between 1 and 12"
