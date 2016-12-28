{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell 	   	 #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module ClientAPI
    ( runQuery
    ) where

import            Data.Aeson
import            Data.Maybe
import            Data.Proxy
import            Data.Time
import            GHC.Generics
import            Network.HTTP.Client (newManager, defaultManagerSettings)
import            Servant.API
import            Servant.Client
import            System.Directory

import            APIs
import            Cache

-- | File Server Stuff

uploadFile :: File -> ClientM ResponseData
getFiles :: ClientM [String]
downloadFile :: String -> ClientM File
getModifyTime :: String -> ClientM UTCTime

fileserverApi :: Proxy FileServerAPI
fileserverApi = Proxy

uploadFile :<|> getFiles :<|> downloadFile :<|> getModifyTime = client fileserverApi

uploadQuery :: String -> String -> ClientM(ResponseData)
uploadQuery fileName fileContents = do
  upload_file <- uploadFile (File fileName fileContents)
  return (upload_file)

downloadQuery :: String -> ClientM(File)
downloadQuery fileName = do
  download_file <- downloadFile (fileName)
  return (download_file)

getFilesQuery :: ClientM([String])
getFilesQuery = do
  get_files <- getFiles
  return (get_files)

-- TODO: Might need to change the return type to return something
runQuery :: String -> String -> String -> IO()
runQuery queryType fileName fileContents = do
  putStrLn "Running Query..."
  manager <- newManager defaultManagerSettings
  case queryType of
    "upload" -> do
      putStrLn $ "Uploading file: " ++ fileName
      res <- runClientM (uploadQuery fileName fileContents) (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
      case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (upload_file) -> do
          print upload_file
    "listfiles" -> do
      putStrLn $ "Getting list of files in directory..."
      res <- runClientM getFileListQuery (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
      case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (get_files) -> do
          print get_files
    "download" -> do
      putStrLn $ "Checking if file is already in cache: " ++ fileName
      isCached <- doesFileExist fileName
      case isCached of
        False -> do
          putStrLn $ "Attempting to download file from server: " ++ fileName
          serverPort <- searchForFileQuery fileName
          case serverPort of
            Nothing -> do
              putStrLn "Unable to find file on directory server"
              return ()
            Just serverPort' -> do
              res <- runClientM (downloadQuery fileName) (ClientEnv manager (BaseUrl Http "localhost" serverPort' ""))
              case res of
                Left err -> putStrLn $ "Error: " ++ show err
                Right (download_file) -> do
                  storeNewFileInCache download_file
                  print download_file
        True -> do
          putStrLn $ "Retrieving file from cache..."
          download_file <- getFileFromCache fileName
          print download_file
    _ -> do
      putStrLn "Invalid Command."

-- | Directry Server Stuff

searchForFile :: String -> ClientM Int
getFileList :: ClientM [String]
updateList :: String -> Int -> String -> ClientM ResponseData

directoryServerApi :: Proxy DirectoryServerAPI
directoryServerApi = Proxy

searchForFile :<|> getFileList :<|> updateList = client directoryServerApi

searchForFileQuery :: String -> IO (Maybe Int)
searchForFileQuery fileName = do
  manager <- newManager defaultManagerSettings
  res <- runClientM (searchForFile fileName) (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return Nothing
    Right (server_port) -> do
      return (Just server_port)

getFileListQuery :: ClientM [String]
getFileListQuery = do
  fileList <- getFileList
  return fileList

