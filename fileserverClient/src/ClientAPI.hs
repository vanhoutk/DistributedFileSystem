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

uploadQuery :: String -> String -> String -> ClientM(SecureResponseData)
uploadQuery ticket fileName fileContents = do
  upload_file <- uploadFile (SecureFileUpload ticket (File fileName fileContents))
  return (upload_file)

downloadQuery :: String -> String -> ClientM(SecureFile)
downloadQuery ticket fileName = do
  download_file <- downloadFile (SecureFileName ticket fileName)
  return (download_file)

getFilesQuery :: ClientM([String])
getFilesQuery = do
  get_files <- getFiles
  return (get_files)

-- TODO: Might need to change the return type to return something
runQuery :: AuthToken -> String -> String -> String -> IO()
runQuery token@(AuthToken decTicket decSessionKey) queryType fileName fileContents = do
  putStrLn "Running Query..."

  let encFileName = encryptDecrypt decSessionKey fileName
  let encFileContents = encryptDecrypt decSessionKey fileName

  manager <- newManager defaultManagerSettings
  case queryType of
    "upload" -> do
      putStrLn $ "Uploading file: " ++ fileName
      res <- runClientM (uploadQuery decTicket encFileName encFileContents) (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
      case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (uploadFileResponse@(SecureResponseData encResponse)) -> do
          let decResponse = encryptDecrypt decSessionKey encResponse
          putStrLn $ "Encrypted upload response: " ++ encResponse
          putStrLn $ "Decrypted upload response: " ++ decResponse
    "listfiles" -> do
      putStrLn $ "Getting list of files in directory..."
      res <- runClientM (getFileListQuery decTicket) (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
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
          serverPort <- searchForFileQuery token fileName
          case serverPort of
            Nothing -> do
              putStrLn "Unable to find file on directory server"
              return ()
            Just serverPort' -> do
              res <- runClientM (downloadQuery decTicket encFileName) (ClientEnv manager (BaseUrl Http "localhost" serverPort' ""))
              case res of
                Left err -> putStrLn $ "Error: " ++ show err
                Right (downloadFile) -> do
                  let (SecureFile (File name contents)) = downloadFile
                  let decName = encryptDecrypt decSessionKey name
                  let decContents = encryptDecrypt decSessionKey contents
                  let decryptedFile = (File decName decContents)
                  storeNewFileInCache decryptedFile
                  print decryptedFile
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

getFileListQuery :: String -> ClientM [String]
getFileListQuery ticket = do
  fileList <- getFileList (SecureTicket ticket)
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

loginClient :: IO(Maybe AuthToken)
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
      let (AuthToken encTicket encSessionKey) = authToken
      let decTicket = encryptDecrypt password encTicket
      let decSessionKey = encryptDecrypt password encSessionKey
      return (Just (AuthToken decTicket decSessionKey))