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
    , setupCache
    , clearCache
    ) where

import            Data.Aeson
import            Data.Maybe
import            Data.Proxy
import            GHC.Generics
import            Network.HTTP.Client (newManager, defaultManagerSettings)
import            Servant.API
import            Servant.Client
import            System.Directory
import            FileserverAPI

-- | File Server Stuff

uploadFile :: File -> ClientM ResponseData
getFiles :: ClientM [String]
downloadFile :: String -> ClientM File

api :: Proxy FileServerAPI
api = Proxy

uploadFile :<|> getFiles :<|> downloadFile = client api

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
      res <- runClientM getFilesQuery (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
      case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (get_files) -> do
          print get_files
    "download" -> do
      putStrLn $ "Downloading file: " ++ fileName
      res <- runClientM (downloadQuery fileName) (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
      case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (download_file) -> do
          storeFileInCache download_file
          print download_file
    _ -> do
      putStrLn "Invalid Command."


-- | Cache Stuff
setupCache :: IO()
setupCache = do
  putStrLn "Initialising client-side cache..."
  createDirectoryIfMissing True ("temp/")
  putStrLn "Changing current directory..."
  setCurrentDirectory ("temp/")

storeFileInCache :: File -> IO()
storeFileInCache (File name contents) = do
  putStrLn $ "Storing file in cache: " ++ name
  writeFile name contents

removeFileFromCache :: String -> IO()
removeFileFromCache fileName = do
  putStrLn $ "Removing file from cache: " ++ fileName
  removeFile fileName

clearCache :: IO()
clearCache = do
  putStrLn "Clearing client-side cache..."
  removeDirectory("../temp/")