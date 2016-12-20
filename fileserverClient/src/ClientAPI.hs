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
    , CacheList
    ) where

import            Control.Concurrent
import            Control.Concurrent.STM
import            Data.Aeson
import            Data.List as L
import            Data.Map as M hiding (foldr, filter, map)
import            Data.Maybe
import            Data.Proxy
import            Data.Time.Clock
import            GHC.Generics
import            Network.HTTP.Client (newManager, defaultManagerSettings)
import            Servant.API
import            Servant.Client
import            System.Directory
import            APIs

maxCacheSize :: Int
maxCacheSize = 4

-- | File Server Stuff

uploadFile :: File -> ClientM ResponseData
getFiles :: ClientM [String]
downloadFile :: String -> ClientM File

fileserverApi :: Proxy FileServerAPI
fileserverApi = Proxy

uploadFile :<|> getFiles :<|> downloadFile = client fileserverApi

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
runQuery :: String -> String -> String -> TVar Int -> CacheList -> IO()
runQuery queryType fileName fileContents cacheSize cacheList = do
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
      putStrLn $ "Checking if file is already in cache: " ++ fileName
      isCached <- doesFileExist fileName
      case isCached of
        False -> do
          putStrLn $ "Attempting to download file from server: " ++ fileName
          res <- runClientM (downloadQuery fileName) (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
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


-- | Cache Stuff
setupCache :: IO()
setupCache = do
  putStrLn "Initialising client-side cache..."
  createDirectoryIfMissing True ("temp/")
  putStrLn "Changing current directory..."
  setCurrentDirectory ("temp/")

storeNewFileInCache :: File -> IO()
storeNewFileInCache (File name contents) = do
  putStrLn "Checking current size of cache..."
  files <- listDirectory("../temp/")
  let cacheSize = length files
  putStrLn $ "Current size of cache is: " ++ show cacheSize
  if cacheSize >= 4
    then do
      putStrLn "Removing oldest file from cache"
      getOldestFileInCache files
      --removeFileFromCache file
      putStrLn $ "Storing file in cache: " ++ name
      writeFile name contents
    else do
      putStrLn $ "Storing file in cache: " ++ name
      writeFile name contents


getFileFromCache :: String -> IO(File)
getFileFromCache fileName = do
  contents <- readFile fileName
  return (File fileName contents)

removeFileFromCache :: String -> IO()
removeFileFromCache fileName = do
  putStrLn $ "Removing file from cache: " ++ fileName
  removeFile fileName

clearCache :: IO()
clearCache = do
  putStrLn "Clearing client-side cache..."
  removeDirectoryRecursive("../temp/")

getOldestFileInCache :: [String] -> IO()
getOldestFileInCache files = do
  putStrLn "getOldestFileInCache"
  mapM_ getFileTime files
  {-
   | TODO:  Figure out how to get the time and name of the least 
            recently used file. Once name is found, can call 
            removeFileFromCache function
   -}
  --maxTime <- L.maximum fileTimes
  --minTime <- L.minimum fileTimes
  --putStrLn $ "Max Time: " ++ show maxTime ++ " Min Time: " ++ show minTime
  return ()

getFileTime :: String -> IO({-UTCTime-})
getFileTime file = do
  time <- getModificationTime file
  putStrLn $ "Filename: " ++ file ++ " Time: " ++ show time
  return ()
  --return time


-- type CacheList = TVar (Map String File)