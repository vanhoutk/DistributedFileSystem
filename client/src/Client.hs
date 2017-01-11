module Client where

import Data.Map as M hiding (filter, map)
import System.IO

import APIs
import Cache
import ClientAPI

startClient :: IO()
startClient = do
  token <- loginClient
  case token of
    Nothing -> do
      putStrLn "Error with username and password. Please try again."
      startClient
    Just token' -> do
      setupCache token'
      clientLoop token'

clientLoop :: AuthToken -> IO()
clientLoop token = do
  putStrLn "Please select command: upload/list_files/download/close_client"
  user_input <- getLine
  case head (words user_input) of
    "upload" -> processUpload token
    "download" -> processDownload token
    "list_files" -> do
      runQuery token "listfiles" "fileName" "fileContents"
      clientLoop token
    "close_client" -> do
      clearCache
    _ -> do
      putStrLn "Invalid commmand. Please try again."
      clientLoop token

processUpload :: AuthToken -> IO()
processUpload token = do
  putStrLn "Please enter the name of the file to upload"
  fileName <- getLine
  putStrLn "Please enter the contents of the file to upload"
  contents <- getLine
  runQuery token "upload" fileName contents
  clientLoop token

processDownload :: AuthToken -> IO()
processDownload token = do
  putStrLn "Please enter the name of the file you wish to download"
  fileName <- getLine
  putStrLn "Would you like to read or write to this file? Enter: read/write"
  downloadType <- getLine
  runQuery token "download" fileName downloadType
  --file <- downloadFileQuery fileName
  --print file
  clientLoop token