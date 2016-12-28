{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cache 
    ( setupCache
    , storeNewFileInCache
    , getFileFromCache
    , removeFileFromCache
    , clearCache
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import Control.Monad
import Data.List
import Data.Proxy
import Data.Time
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import System.Directory
import System.Environment
import System.IO

import APIs

type FileSet = (Int, UTCTime, FilePath)

maxCacheSize :: Int
maxCacheSize = 50 -- Size is detemined by number of characters. TODO: Make this bigger

setupCache :: IO()
setupCache = do
  putStrLn "Initialising client-side cache..."
  createDirectoryIfMissing True ("temp/")
  putStrLn "Changing current directory..."
  setCurrentDirectory ("temp/")
  forkIO $ checkForUpdate 60
  return ()

checkForUpdate :: Int -> IO()
checkForUpdate delay = do
  threadDelay $ delay * 1000000 -- Wait 1 minute
  putStrLn "Checking Cache for invalidations..."
  fileList <- listDirectory "../temp/"
  mapM_ (checkFileForUpdate) fileList
  checkForUpdate delay -- Tail recursion

checkFileForUpdate :: String -> IO()
checkFileForUpdate fileName = do
  localFileTime <- getAccessTime fileName -- Note: This is getAccessTime as opposed to getModificationTime, while the fileserver uses getModificationTime
  serverPort <- searchForFileQuery fileName
  case serverPort of
    Nothing -> do
      putStrLn "Couldn't find file in Directory Server."
      return ()
    Just serverPort' -> do
      remoteFileTime <- fileModifyTimeQuery serverPort' fileName
      case remoteFileTime of
        Nothing -> do
          putStrLn "Remote file does not exist"
          return ()
        Just time -> do
          if(time >= localFileTime) then do
            putStrLn "Changes made to remote file..."
            file <- downloadFileQuery serverPort' fileName
            case file of
              Nothing -> return()
              Just file' -> do
                storeNewFileInCache file'
                return()
          else do
            putStrLn "No changes made to remote file..."
            return()

-- | Cache stuff

storeNewFileInCache :: File -> IO()
storeNewFileInCache (File name contents) = do
  putStrLn $ "Storing file in cache: " ++ name
  writeFile name contents
  putStrLn "Updating cache..."
  updateCache

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

updateCache :: IO()
updateCache = do
  unsortedCache <- listCache "../temp/"
  let sortedCache = sortBy (\(_,a,_) (_,b,_) -> compare a b) unsortedCache
  print unsortedCache
  print sortedCache
  clearSpaceCache sortedCache

  where
    size = foldl (\acc (fileSize,_,_) -> fileSize + acc) 0 -- Calculates the total size of the list
    clearSpaceCache cache
      | size cache < maxCacheSize = return ()
      | otherwise = do
        let (_,_,filePath) = head cache
        let sizeOfCache = size cache
        print ("Size of cache: " ++ (show sizeOfCache) ++ " Deleting file: " ++ filePath)
        removeFileFromCache filePath
        clearSpaceCache $ tail cache
   
listCache:: FilePath -> IO [FileSet]
listCache path = do
  files <- listDirectory path
  fileSet <- mapM (fileinfo []) files
  return $ concat fileSet
  
  where
    fileinfo:: [FileSet] -> FilePath -> IO [FileSet]
    fileinfo a []   = return a
    fileinfo a filePath =  do 
      let p = path ++ filePath
      act <- getModificationTime p
      szt <- getFileSize p
      let sz = (read :: String -> Int) $ show szt
      isdir <- doesFileExist p 
      if isdir then  return $ (sz, act, p):a 
        else  return a

-- | File server bits
-- TODO: Make this more modular and less hardcoded (w.r.t. port number and localhost)

uploadFile :: File -> ClientM ResponseData
deleteFile :: String -> ClientM ResponseData
getFiles :: ClientM [String]
downloadFile :: String -> ClientM File
getModifyTime :: String -> ClientM UTCTime

fileserverApi :: Proxy FileServerAPI
fileserverApi = Proxy

uploadFile :<|> deleteFile :<|> getFiles :<|> downloadFile :<|> getModifyTime = client fileserverApi

downloadQuery :: String -> ClientM(File)
downloadQuery fileName = do
  download_file <- downloadFile (fileName)
  return (download_file)

downloadFileQuery :: Int -> String -> IO (Maybe File)
downloadFileQuery port fileName = do
  manager <- newManager defaultManagerSettings
  res <- runClientM (downloadQuery fileName) (ClientEnv manager (BaseUrl Http "localhost" port ""))
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return Nothing
    Right (download_file) -> do
      return (Just download_file)

modifyTimeQuery :: String -> ClientM(UTCTime)
modifyTimeQuery fileName = do
  fileModTime <- getModifyTime (fileName)
  return (fileModTime)

fileModifyTimeQuery :: Int -> String -> IO (Maybe UTCTime)
fileModifyTimeQuery port fileName = do
  manager <- newManager defaultManagerSettings
  res <- runClientM (modifyTimeQuery fileName) (ClientEnv manager (BaseUrl Http "localhost" port ""))
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return Nothing
    Right (fileModTime) -> do
      return (Just fileModTime)

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