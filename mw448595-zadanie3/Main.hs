module Main where

import System.Environment
import Parser
import CER

parseOnly :: String -> IO ()
parseOnly = print . fromHsString

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"]          -> usage
    ["--parse"]         -> getContents  >>= parseOnly
    ["--parse", file]   -> readFile file >>= parseOnly
    []                  -> getContents  >>= runString
    [file]              -> readFile file >>= runString
    _                   -> usage

usage :: IO ()
usage = do
  putStrLn "Usage: program [--help] [--parse [file]] [file]"
  putStrLn "  --help          display this message"
  putStrLn "  --parse [file]  run only the Parser and print the AST"
  putStrLn "  file            input file (defaults to stdin)"
