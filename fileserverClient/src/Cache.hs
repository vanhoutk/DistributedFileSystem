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

setupCache :: AuthToken -> IO()
setupCache token = do
  putStrLn "Initialising client-side cache..."
  createDirectoryIfMissing True ("temp/")
  putStrLn "Changing current directory..."
  setCurrentDirectory ("temp/")
  forkIO $ checkForUpdate token 60
  return ()

checkForUpdate :: AuthToken -> Int -> IO()
checkForUpdate token delay = do
  threadDelay $ delay * 1000000 -- Wait 1 minute
  putStrLn "Checking Cache for invalidations..."
  fileList <- listDirectory "../temp/"
  mapM_ (checkFileForUpdate token) fileList
  checkForUpdate token delay -- Tail recursion

checkFileForUpdate :: AuthToken -> String -> IO()
checkFileForUpdate token@(AuthToken decTicket decSessionKey) fileName = do
  localFileTime <- getAccessTime fileName -- Note: This is getAccessTime as opposed to getModificationTime, while the fileserver uses getModificationTime
  serverPort <- searchForFileQuery token fileName
  case serverPort of
    Nothing -> do
      putStrLn "Couldn't find file in Directory Server."
      return ()
    Just serverPort' -> do
      remoteFileTime <- fileModifyTimeQuery token serverPort' fileName
      case remoteFileTime of
        Nothing -> do
          putStrLn "Remote file does not exist"
          return ()
        Just time -> do
          if(time >= localFileTime) then do
            putStrLn "Changes made to remote file..."
            file <- downloadFileQuery token serverPort' fileName
            case file of
              Nothing -> return()
              Just file' -> do
                let (SecureFile (File name contents)) = file'
                let decName = encryptDecrypt decSessionKey name
                let decContents = encryptDecrypt decSessionKey contents
                let decryptedFile = (File decName decContents)
                storeNewFileInCache decryptedFile
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

uploadFile :: SecureFileUpload -> ClientM SecureResponseData
deleteFile :: SecureFileName -> ClientM SecureResponseData
getFiles :: ClientM [String]
downloadFile :: SecureFileName -> ClientM SecureFile
getModifyTime :: SecureFileName -> ClientM SecureTime

fileserverApi :: Proxy FileServerAPI
fileserverApi = Proxy

uploadFile :<|> deleteFile :<|> getFiles :<|> downloadFile :<|> getModifyTime = client fileserverApi

downloadQuery :: String -> String -> ClientM(SecureFile)
downloadQuery ticket fileName = do
  download_file <- downloadFile (SecureFileName ticket fileName)
  return (download_file)

downloadFileQuery :: AuthToken -> Int -> String -> IO (Maybe SecureFile)
downloadFileQuery token@(AuthToken decTicket decSessionKey) port fileName = do
  let encFileName = encryptDecrypt decSessionKey fileName
  manager <- newManager defaultManagerSettings
  res <- runClientM (downloadQuery decTicket encFileName) (ClientEnv manager (BaseUrl Http "localhost" port ""))
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return Nothing
    Right (download_file) -> do
      return (Just download_file)

modifyTimeQuery :: String -> String -> ClientM(SecureTime)
modifyTimeQuery ticket fileName = do
  fileModTime <- getModifyTime (SecureFileName ticket fileName)
  return (fileModTime)

fileModifyTimeQuery :: AuthToken -> Int -> String -> IO (Maybe UTCTime)
fileModifyTimeQuery token@(AuthToken decTicket decSessionKey) port fileName = do
  let encFileName = encryptDecrypt decSessionKey fileName
  manager <- newManager defaultManagerSettings
  res <- runClientM (modifyTimeQuery decTicket encFileName) (ClientEnv manager (BaseUrl Http "localhost" port ""))
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return Nothing
    Right (SecureTime encTime) -> do
      let decTime = decryptTime decSessionKey encTime
      return (Just decTime)

-- | Directry Server Stuff

searchForFile :: SecureFileName -> ClientM SecurePort
getFileList :: SecureTicket -> ClientM [String]
updateList :: String -> Int -> String -> ClientM ResponseData

directoryServerApi :: Proxy DirectoryServerAPI
directoryServerApi = Proxy

searchForFile :<|> getFileList :<|> updateList = client directoryServerApi

searchQuery :: String -> String -> ClientM SecurePort
searchQuery ticket fileName = do
  searchResult <- searchForFile (SecureFileName ticket fileName)
  return searchResult

searchForFileQuery :: AuthToken -> String -> IO (Maybe Int)
searchForFileQuery token@(AuthToken decTicket decSessionKey) fileName = do
  let encFileName = encryptDecrypt decSessionKey fileName
  manager <- newManager defaultManagerSettings
  res <- runClientM (searchQuery decTicket encFileName) (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return Nothing
    Right (SecurePort encPort) -> do
      let decPort = decryptPort decSessionKey encPort
      return (Just decPort)