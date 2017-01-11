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
    , loginClient
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

uploadFile :: SecureFileUpload -> ClientM SecureResponseData
deleteFile :: SecureFileName -> ClientM SecureResponseData
getFiles :: ClientM [String]
downloadFile :: SecureFileName -> ClientM SecureFile
getModifyTime :: SecureFileName -> ClientM SecureTime

fileserverApi :: Proxy FileServerAPI
fileserverApi = Proxy

uploadFile :<|> deleteFile :<|> getFiles :<|> downloadFile :<|> getModifyTime = client fileserverApi

uploadQuery :: String -> String -> String -> String -> ClientM SecureResponseData
uploadQuery ticket encTimeOut fileName fileContents = do
  upload_file <- uploadFile (SecureFileUpload ticket encTimeOut (File fileName fileContents))
  return (upload_file)

downloadQuery :: String -> String -> String -> ClientM SecureFile
downloadQuery ticket encTimeOut fileName = do
  download_file <- downloadFile (SecureFileName ticket encTimeOut fileName)
  return (download_file)

downloadQ :: AuthToken -> String -> IO ()
downloadQ token@(AuthToken decTicket decSessionKey encTimeOut) fileName = do
  putStrLn $ "Encrypted File Name: " ++ fileName
  let encFileName = encryptDecrypt decSessionKey fileName
  serverPort <- searchForFileQuery token fileName
  manager <- newManager defaultManagerSettings
  case serverPort of
    Nothing -> do
      putStrLn "Unable to find file on directory server"
      return ()
    Just serverPort' -> do
      res <- runClientM (downloadQuery decTicket encTimeOut encFileName) (ClientEnv manager (BaseUrl Http "localhost" serverPort' ""))
      case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (downloadFile) -> do
          let (SecureFile (File name contents)) = downloadFile
          let decName = encryptDecrypt decSessionKey name
          let decContents = encryptDecrypt decSessionKey contents
          let decryptedFile = (File decName decContents)
          storeNewFileInCache decryptedFile
          print decryptedFile

getFilesQuery :: ClientM([String])
getFilesQuery = do
  get_files <- getFiles
  return (get_files)

-- TODO: Might need to change the return type to return something
runQuery :: AuthToken -> String -> String -> String -> IO()
runQuery token@(AuthToken decTicket decSessionKey encTimeOut) queryType fileName contentsOrType = do
  putStrLn "Running Query..."

  let encFileName = encryptDecrypt decSessionKey fileName
  let encFileContents = encryptDecrypt decSessionKey contentsOrType

  manager <- newManager defaultManagerSettings
  case queryType of
    "upload" -> do
      putStrLn $ "Uploading file: " ++ fileName
      res <- runClientM (uploadQuery decTicket encTimeOut encFileName encFileContents) (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
      case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (uploadFileResponse@(SecureResponseData encResponse)) -> do
          let decResponse = encryptDecrypt decSessionKey encResponse
          putStrLn $ "Encrypted upload response: " ++ encResponse
          putStrLn $ "Decrypted upload response: " ++ decResponse
    "listfiles" -> do
      putStrLn $ "Getting list of files in directory..."
      res <- runClientM (getFileListQuery decTicket encTimeOut) (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
      case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (encFiles) -> do
          let decFiles = encryptDecryptArray decSessionKey encFiles
          print decFiles
    "download" -> do
      putStrLn $ "Checking if file is already in cache: " ++ fileName
      isCached <- doesFileExist fileName
      case isCached of
        False -> do
          putStrLn $ "Attempting to download file from server: " ++ fileName
          case contentsOrType of
            "write" -> do
              isLocked <- checkLockF token fileName
              case isLocked of
                Nothing -> do
                  putStrLn "Error when checking lock on file."
                Just isLocked' -> do
                  case isLocked' of
                    False -> do
                      lockF token fileName
                      downloadQ token fileName
                    True -> do
                      putStrLn "Unable to get write access to this file. There is already a lock on it."
            "read" -> do
              downloadQ token fileName
        True -> do
          putStrLn $ "Retrieving file from cache..."
          downloadFile <- getFileFromCache fileName
          print downloadFile
    _ -> do
      putStrLn "Invalid Command."

-- | Directry Server Stuff

searchForFile :: SecureFileName -> ClientM SecurePort
getFileList :: SecureTicket -> ClientM [String]
updateList :: String -> Int -> String -> ClientM ResponseData

directoryServerApi :: Proxy DirectoryServerAPI
directoryServerApi = Proxy

searchForFile :<|> getFileList :<|> updateList = client directoryServerApi

searchQuery :: String -> String -> String -> ClientM SecurePort
searchQuery ticket encTimeOut fileName = do 
  searchResult <- searchForFile (SecureFileName ticket encTimeOut fileName)
  return searchResult

searchForFileQuery :: AuthToken -> String -> IO (Maybe Int)
searchForFileQuery token@(AuthToken decTicket decSessionKey encTimeOut) fileName = do
  let encFileName = encryptDecrypt decSessionKey fileName
  manager <- newManager defaultManagerSettings
  res <- runClientM (searchQuery decTicket encTimeOut encFileName) (ClientEnv manager (BaseUrl Http "localhost" directoryPort ""))
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return Nothing
    Right (SecurePort encPort) -> do
      let decPort = decryptPort decSessionKey encPort
      return (Just decPort)

getFileListQuery :: String -> String -> ClientM [String]
getFileListQuery ticket encTimeOut = do
  fileList <- getFileList (SecureTicket ticket encTimeOut)
  return fileList

-- | Authentication Stuff

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

{-addNewUserQuery :: String -> String -> ClientM ResponseData 
addNewUserQuery username password = do
  response <- addNewUser username password
  return response-}

loginClient :: IO (Maybe AuthToken)
loginClient = do
  putStrLn "Please enter username: "
  username <- getLine
  putStrLn "Please enter password: "
  password <- getLine
  manager <- newManager defaultManagerSettings
  res <- runClientM (loginUserQuery username password) (ClientEnv manager (BaseUrl Http "localhost" asPort ""))
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return Nothing
    Right (authToken) -> do
      let (AuthToken encTicket encSessionKey encTimeOut) = authToken
      case encTicket of
        "Failed" -> do
          putStrLn $ "Failed with error: " ++ encSessionKey
          return Nothing
        _ -> do
          let decTicket = encryptDecrypt password encTicket
          let decSessionKey = encryptDecrypt password encSessionKey
          return (Just (AuthToken decTicket decSessionKey encTimeOut))

-- | Lock Server Stuff

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
  res <- runClientM (lockQuery decTicket encTimeOut encFileName) (ClientEnv manager (BaseUrl Http "localhost" lsPort ""))
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return ()
    Right (SecureResponseData encResponse) -> do
      let decResonse = encryptDecrypt decSessionKey encResponse
      putStrLn $ "Locking Response: " ++ decResonse
      return ()

unlockQuery :: String -> String -> String -> ClientM SecureResponseData
unlockQuery ticket encTimeOut fileName = do
  unlockQ <- unlockFile (SecureFileName ticket encTimeOut fileName)
  return unlockQ

unlockF :: AuthToken -> String -> IO ()
unlockF token@(AuthToken decTicket decSessionKey encTimeOut) fileName = do
  let encFileName = encryptDecrypt decSessionKey fileName
  manager <- newManager defaultManagerSettings
  res <- runClientM (unlockQuery decTicket encTimeOut encFileName) (ClientEnv manager (BaseUrl Http "localhost" lsPort ""))
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return ()
    Right (SecureResponseData encResponse) -> do
      let decResonse = encryptDecrypt decSessionKey encResponse
      putStrLn $ "Unlocking Response: " ++ decResonse
      return ()

checkLockQuery :: String -> String -> String -> ClientM Bool
checkLockQuery ticket encTimeOut fileName = do
  checkLockQ <- checkLockFile (SecureFileName ticket encTimeOut fileName)
  return checkLockQ

checkLockF :: AuthToken -> String -> IO (Maybe Bool)
checkLockF token@(AuthToken decTicket decSessionKey encTimeOut) fileName = do
  let encFileName = encryptDecrypt decSessionKey fileName
  manager <- newManager defaultManagerSettings
  res <- runClientM (checkLockQuery decTicket encTimeOut encFileName) (ClientEnv manager (BaseUrl Http "localhost" lsPort ""))
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return Nothing
    Right (isLocked) -> do
      return (Just isLocked)