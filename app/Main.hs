module Main where

import Sudoku (loadSudokuFile, solve)

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)


parseArgs :: IO String
parseArgs = do
  args <- getArgs
  case args of
    [arg] -> return arg
    []     -> do hPutStrLn stderr
                   ("Missing argument. Expects the file path to a sudoku.")
                 exitFailure

    _      -> do hPutStrLn stderr
                   ("Too many parameters. Only one argument permitted.")
                 exitFailure


main :: IO ()
main = do
  filename <- parseArgs

  x <- maybe [] solve <$> loadSudokuFile filename

  case x of
    []      -> return ()
    (x':_)  -> putStrLn $ show x'
