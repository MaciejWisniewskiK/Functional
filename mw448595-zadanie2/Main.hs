module Main where

import CER
import System.Environment

-- https://github.com/mbenke/pf25/blob/main/w05io.md
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []  -> getContents >>= runString
    [f] -> readFile f >>= runString 
    _   -> usage

usage :: IO ()
usage = do
  putStrLn "Usage: program [--help] [file]"
  putStrLn "  --help  - display this message"
  putStrLn "  file    - input file"