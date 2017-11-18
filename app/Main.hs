module Main where

import Lib
import Spreadsheet.Reader
import System.Environment    (getEnv, getArgs)
import Data.Text

main :: IO ()
main = do
  args <- getArgs
  let sheetId = Prelude.head args
      ranges = Prelude.last args
  ranges' <- fetchValueRanges sheetId ranges
  let vs = values $ Prelude.head ranges'
  printAllValues vs

printAllValues :: [[Text]] -> IO ()
printAllValues [x] = printValue x
printAllValues (x:xs) = do
  let x = Prelude.head xs
  printValue x
  printAllValues xs

printValue :: [Text] -> IO ()
printValue x = do
  let tl = Prelude.tail x
  putStrLn $ Data.Text.unpack $ Prelude.head x
  putStrLn $ Data.Text.unpack $ Prelude.head tl
  putStrLn $ Data.Text.unpack $ Prelude.last tl
