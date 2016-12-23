module Main where

import System.Environment
import System.IO
import Fileserver

main :: IO ()
main = do
  args <- getArgs
  let port = read $ head args :: Int
  startServer port
