module Client where

import Data.Map as M hiding (filter, map)
import System.IO

import APIs
import Cache
import ClientAPI
import ClientEditor

startClient :: IO()
startClient = do
  token <- loginClient
  case token of
    Nothing -> do
      putStrLn "Error with username and password. Please try again."
      startClient
    Just token' -> do
      putStrLn "Login successful. Starting Text Editor..."
      setupCache token'
      startEditor token'