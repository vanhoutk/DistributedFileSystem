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
    ( downloadReadWriteQ
    , fileListQuery
    , loginClient
    , uploadToServerQuery
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


-- | File Server

uploadFile :: SecureFileUpload -> ClientM SecureResponseData
deleteFile :: SecureFileName -> ClientM SecureResponseData
getFiles :: ClientM [String]
downloadFile :: SecureFileName -> ClientM SecureFile
getModifyTime :: SecureFileName -> ClientM SecureTime

fileserverApi :: Proxy FileServerAPI
fileserverApi = Proxy

uploadFile :<|> deleteFile :<|> getFiles :<|> downloadFile :<|> getModifyTime = client fileserverApi

downloadQuery :: String -> String -> String -> ClientM SecureFile
downloadQuery ticket encTimeOut fileName = do
  download_file <- downloadFile (SecureFileName ticket encTimeOut fileName)
  return (download_file)

downloadQ :: AuthToken -> String -> IO (Maybe File)
downloadQ token@(AuthToken decTicket decSessionKey encTimeOut) fileName = do
  logMessage clientLogging ("Attempting to download file: " ++ fileName)

  let encFileName = encryptDecrypt decSessionKey fileName
  serverPort <- searchForFileQuery token fileName
  
  manager <- newManager defaultManagerSettings
  case serverPort of
    Nothing -> do
      logMessage clientLogging ("Error - Unable to find file on directory server")
      return Nothing
    Just serverPort' -> do
      res <- runClientM (downloadQuery decTicket encTimeOut encFileName) (ClientEnv manager (BaseUrl Http host serverPort' ""))
      case res of
        Left err -> do
          logMessage clientLogging ("Error downloading file: " ++ show err)
          return Nothing
        Right (downloadFile) -> do
          let (SecureFile (File name contents)) = downloadFile
          let decName = encryptDecrypt decSessionKey name
          let decContents = encryptDecrypt decSessionKey contents
          let decryptedFile = (File decName decContents)
          storeNewFileInCache decryptedFile
          return (Just decryptedFile)

downloadReadWriteQ :: AuthToken -> String -> String -> IO (Maybe File)
downloadReadWriteQ token@(AuthToken decTicket decSessionKey encTimeOut) fileName readOrWrite = do
  case readOrWrite of
    "write" -> do
      logMessage clientLogging ("Checking for lock on file: " ++ fileName)
      isLocked <- checkLockF token fileName
      case isLocked of
        Nothing -> do
          logMessage clientLogging ("Error when checking lock on file.")
          return Nothing
        Just isLocked' -> do
          case isLocked' of
            False -> do
              lockF token fileName
              logMessage clientLogging ("Checking if file is already in cache: " ++ fileName)
              isCached <- doesFileExist fileName
              case isCached of
                False -> do
                  logMessage clientLogging ("File not in cache, will attempt to download...")
                  file <- downloadQ token fileName
                  case file of
                    Nothing -> return Nothing
                    Just file' -> return (Just file')
                
                True -> do
                  logMessage clientLogging ("File found in cache. Retrieving file...")
                  cachedFile <- getFileFromCache fileName
                  return (Just cachedFile)
            
            True -> do
              putStrLn "Unable to get write access to this file. There is already a lock on it."
              return Nothing
    
    "read" -> do
      logMessage clientLogging ("Checking if file is already in cache: " ++ fileName)
      isCached <- doesFileExist fileName
      case isCached of
        False -> do
          logMessage clientLogging ("File not in cache, will attempt to download...")
          file <- downloadQ token fileName
          case file of
            Nothing -> return Nothing
            Just file' -> return (Just file')
        
        True -> do
          logMessage clientLogging ("File found in cache. Retrieving file...")
          cachedFile <- getFileFromCache fileName
          return (Just cachedFile)

-- | Directory Server Stuff

searchForFile :: SecureFileName -> ClientM SecurePort
uploadToServer :: SecureFileUpload -> ClientM SecureResponseData
getFileList :: SecureTicket -> ClientM [String]
updateList :: String -> Int -> String -> ClientM ResponseData

directoryServerApi :: Proxy DirectoryServerAPI
directoryServerApi = Proxy

searchForFile :<|> uploadToServer :<|> getFileList :<|> updateList = client directoryServerApi

searchQuery :: String -> String -> String -> ClientM SecurePort
searchQuery ticket encTimeOut fileName = do 
  searchResult <- searchForFile (SecureFileName ticket encTimeOut fileName)
  return searchResult

searchForFileQuery :: AuthToken -> String -> IO (Maybe Int)
searchForFileQuery token@(AuthToken decTicket decSessionKey encTimeOut) fileName = do
  let encFileName = encryptDecrypt decSessionKey fileName
  manager <- newManager defaultManagerSettings
  res <- runClientM (searchQuery decTicket encTimeOut encFileName) (ClientEnv manager (BaseUrl Http host dsPort ""))
  case res of
    Left err -> do
      logMessage clientLogging ("Error searching for file: " ++ show err)
      return Nothing
    Right (SecurePort encPort) -> do
      let decPort = decryptPort decSessionKey encPort
      return (Just decPort)

getFileListQuery :: String -> String -> ClientM [String]
getFileListQuery ticket encTimeOut = do
  fileList <- getFileList (SecureTicket ticket encTimeOut)
  return fileList

fileListQuery :: AuthToken -> IO (Maybe [String])
fileListQuery token@(AuthToken decTicket decSessionKey encTimeOut) = do
  logMessage clientLogging ("Requesting list of files from Directory Server...")
  manager <- newManager defaultManagerSettings
  res <- runClientM (getFileListQuery decTicket encTimeOut) (ClientEnv manager (BaseUrl Http host 8080 ""))
  case res of
    Left err -> do
      logMessage clientLogging ("Error getting list of files: " ++ show err)
      return Nothing
    Right (encFiles) -> do
      let decFiles = encryptDecryptArray decSessionKey encFiles
      print decFiles
      return (Just decFiles)

uploadServerQuery :: String -> String -> String -> String -> ClientM SecureResponseData
uploadServerQuery ticket encTimeOut fileName fileContents = do
  upload_file <- uploadFile (SecureFileUpload ticket encTimeOut (File fileName fileContents))
  return (upload_file)

uploadToServerQuery :: AuthToken -> String -> String -> IO ()
uploadToServerQuery token@(AuthToken decTicket decSessionKey encTimeOut) fileName contents = do
  logMessage clientLogging ("Uploading File: " ++ fileName)
  unlockF token fileName
  let encFileName = encryptDecrypt decSessionKey fileName
  let encFileContents = encryptDecrypt decSessionKey contents
  manager <- newManager defaultManagerSettings
  res <- runClientM (uploadServerQuery decTicket encTimeOut encFileName encFileContents) (ClientEnv manager (BaseUrl Http host dsPort ""))
  case res of
    Left err -> logMessage clientLogging ("Error uploading file: " ++ show err)
    Right (uploadFileResponse@(SecureResponseData encResponse)) -> do
      let decResponse = encryptDecrypt decSessionKey encResponse
      putStrLn $ "Decrypted upload response: " ++ decResponse

-- | Authentication Server

loginUser :: LoginRequest -> ClientM AuthToken
addNewUser :: String -> String -> ClientM ResponseData

authenticationServerAPI :: Proxy AuthenticationServerAPI
authenticationServerAPI = Proxy

loginUser :<|> addNewUser = client authenticationServerAPI

loginUserQuery :: String -> String -> ClientM AuthToken
loginUserQuery username password = do
  let encUsername = encryptDecrypt password username
  authToken <- loginUser (LoginRequest username encUsername)
  return authToken

loginClient :: IO (Maybe AuthToken)
loginClient = do
  putStrLn "Please enter username: "
  username <- getLine
  putStrLn "Please enter password: "
  password <- getLine
  manager <- newManager defaultManagerSettings
  res <- runClientM (loginUserQuery username password) (ClientEnv manager (BaseUrl Http host asPort ""))
  case res of
    Left err -> do
      logMessage clientLogging ("Error logging in user: " ++ show err)
      return Nothing
    Right (authToken) -> do
      let (AuthToken encTicket encSessionKey encTimeOut) = authToken
      case encTicket of
        "Failed" -> do
          putStrLn $ "Unable to log in. Error: " ++ encSessionKey
          return Nothing
        _ -> do
          logMessage clientLogging ("Login Successful.")
          let decTicket = encryptDecrypt password encTicket
          let decSessionKey = encryptDecrypt password encSessionKey
          return (Just (AuthToken decTicket decSessionKey encTimeOut))


-- | Lock Server

lockFile :: SecureFileName -> ClientM SecureResponseData
unlockFile :: SecureFileName -> ClientM SecureResponseData
checkLockFile :: SecureFileName -> ClientM Bool

lockServerAPI :: Proxy LockServerAPI
lockServerAPI = Proxy

lockFile :<|> unlockFile :<|> checkLockFile = client lockServerAPI

lockQuery :: String -> String -> String -> ClientM SecureResponseData
lockQuery ticket encTimeOut fileName = do
  lockQ <- lockFile (SecureFileName ticket encTimeOut fileName)
  return lockQ

lockF :: AuthToken -> String -> IO ()
lockF token@(AuthToken decTicket decSessionKey encTimeOut) fileName = do
  let encFileName = encryptDecrypt decSessionKey fileName
  manager <- newManager defaultManagerSettings
  res <- runClientM (lockQuery decTicket encTimeOut encFileName) (ClientEnv manager (BaseUrl Http host lsPort ""))
  case res of
    Left err -> do
      logMessage clientLogging ("Error locking file: " ++ show err)
      return ()
    Right (SecureResponseData encResponse) -> do
      let decResponse = encryptDecrypt decSessionKey encResponse
      logMessage clientLogging ("Response from Lock Server: " ++ decResponse)
      return ()

unlockQuery :: String -> String -> String -> ClientM SecureResponseData
unlockQuery ticket encTimeOut fileName = do
  unlockQ <- unlockFile (SecureFileName ticket encTimeOut fileName)
  return unlockQ

unlockF :: AuthToken -> String -> IO ()
unlockF token@(AuthToken decTicket decSessionKey encTimeOut) fileName = do
  let encFileName = encryptDecrypt decSessionKey fileName
  manager <- newManager defaultManagerSettings
  res <- runClientM (unlockQuery decTicket encTimeOut encFileName) (ClientEnv manager (BaseUrl Http host lsPort ""))
  case res of
    Left err -> do
      logMessage clientLogging ("Error unlocking file: " ++ show err)
      return ()
    Right (SecureResponseData encResponse) -> do
      let decResponse = encryptDecrypt decSessionKey encResponse
      logMessage clientLogging ("Response from Lock Server: " ++ decResponse)
      return ()

checkLockQuery :: String -> String -> String -> ClientM Bool
checkLockQuery ticket encTimeOut fileName = do
  checkLockQ <- checkLockFile (SecureFileName ticket encTimeOut fileName)
  return checkLockQ

checkLockF :: AuthToken -> String -> IO (Maybe Bool)
checkLockF token@(AuthToken decTicket decSessionKey encTimeOut) fileName = do
  let encFileName = encryptDecrypt decSessionKey fileName
  manager <- newManager defaultManagerSettings
  res <- runClientM (checkLockQuery decTicket encTimeOut encFileName) (ClientEnv manager (BaseUrl Http host lsPort ""))
  case res of
    Left err -> do
      logMessage clientLogging ("Error checking lock on file: " ++ show err)
      return Nothing
    Right (isLocked) -> do
      return (Just isLocked)