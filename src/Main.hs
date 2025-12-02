module Main where

import System.Environment (getArgs)

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
runTask n
  | n > 0 && n <= 12 = putStrLn "Day not done yet"
  | otherwise        = putStrLn "Number must be between 1 and 12"
