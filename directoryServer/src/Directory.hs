{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Directory
    ( startDirectory
    ) where

import            Control.Monad.IO.Class
import            Control.Monad.Trans.Except
import            Control.Monad.Trans.Resource
import            Data.Aeson
import            Data.Bson.Generic
import qualified  Data.List                     as DL
import            Data.Maybe
import            Data.Proxy
import            Data.Text                     (pack, unpack)
import            Data.Time
import            Database.MongoDB
import            Network.HTTP.Client           (newManager, defaultManagerSettings)
import            Network.Wai
import            Network.Wai.Handler.Warp
import            Servant
import qualified  Servant.API                   as SC
import qualified  Servant.Client                as SC
import            System.Directory
import            System.Random
import            APIs
import            MongoFunctions

type APIHandler = ExceptT ServantErr IO

fileServerPorts :: [Int]
fileServerPorts = [8081, 8082]

startDirectory :: IO ()
startDirectory = do
  initDirectory fileServerPorts -- Initialise the directory by getting the list of files on all active file servers
  logMessage dirServerLogging ("Starting Directory Server...")
  run dsPort app

initDirectory :: [Int] -> IO()
initDirectory ports = do
  logMessage dirServerLogging ("Getting list of files on active file servers...")
  mapM (fileMapList []) ports
  return ()

  where
    fileMapList :: [FileMapping] -> Int -> IO [FileMapping]
    fileMapList a port = do
      logMessage dirServerLogging ("Attempting to get file list from file server on port " ++ show port ++ " ...")
      files <- runGetFilesQuery port
      case files of
        Nothing -> do
          logMessage dirServerLogging ("Error getting file list...")
          return a
        Just files' -> do
          fileMappingList' <- mapM (fileMap port []) files'
          return $ concat fileMappingList'

    fileMap :: Int -> [FileMapping] -> String -> IO [FileMapping]
    fileMap port a [] = return a
    fileMap port a fileName = do
      let serverNumber = port `mod` 8080
      let serverName = "Server" ++ show serverNumber
      let fileMapping = (FileMapping fileName serverName (show port))
      logMessage dirServerLogging ("Inserting FileMapping(Filename: " ++ fileName ++ " , Servername: " ++ serverName ++ " , Port: " ++ show port ++ ") into database...")
      withMongoDbConnection $ upsert (select ["fileName" =: fileName, "serverName" =: serverName] "FILE_SERVER_MAPPINGS") $ toBSON fileMapping 
      return $ (FileMapping fileName serverName (show port)):a

app :: Application
app = serve api server

api :: Proxy DirectoryServerAPI
api = Proxy

server :: Server DirectoryServerAPI
server = searchForFile
    :<|> findUploadServer
    :<|> listFiles
    :<|> updateLists

  where

    searchForFile :: SecureFileName -> APIHandler SecurePort
    searchForFile (SecureFileName ticket encTimeOut encFileName) = do
      let decTimeOut = decryptTime sharedServerSecret encTimeOut
      let sessionKey = encryptDecrypt sharedServerSecret ticket
      let decFileName = encryptDecrypt sessionKey encFileName

      liftIO $ logMessage dirServerLogging ("Search for file request received for file: " ++ decFileName)

      currentTime <- liftIO $ getCurrentTime
      if (currentTime > decTimeOut) then do
        liftIO $ logMessage dirServerLogging ("Client's authentication token has expired.")
        let encPort = encryptPort sessionKey 0
        return (SecurePort encPort)
      else do
        liftIO $ logMessage dirServerLogging ("Searching for file: " ++ decFileName)
        fileMapping <- getFileMapping decFileName
        case fileMapping of
          Nothing -> do
            liftIO $ logMessage dirServerLogging ("File not found.")
            let encPort = encryptPort sessionKey 0
            return (SecurePort encPort)
          Just fileMapping' -> do
            let (FileMapping _ server port) = fileMapping'
            let port' = (read :: String -> Int) $ port
            liftIO $ logMessage dirServerLogging ("File found on " ++ server ++ " operating on port: " ++ port)
            let encPort = encryptPort sessionKey port'
            return (SecurePort encPort)

      where
        getFileMapping :: String -> APIHandler (Maybe FileMapping)
        getFileMapping name = do
          fileMap <- liftIO $ withMongoDbConnection $ do
            docs <- find (select ["fileName" =: name] "FILE_SERVER_MAPPINGS") >>= drainCursor
            return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileMapping) docs

          case (length fileMap) of -- Currently no replicating so the length should either be 0 or 1
            0 -> return Nothing 
            _ -> return (Just (head fileMap))

    findUploadServer :: SecureFileName -> APIHandler SecurePort
    findUploadServer (SecureFileName ticket encTimeOut encFileName) = do
      let decTimeOut = decryptTime sharedServerSecret encTimeOut
      let sessionKey = encryptDecrypt sharedServerSecret ticket
      let decFileName = encryptDecrypt sessionKey encFileName

      liftIO $ logMessage dirServerLogging ("Searching for server to upload file " ++ decFileName ++ " to...")

      currentTime <- liftIO $ getCurrentTime
      if (currentTime > decTimeOut) then do
        liftIO $ logMessage dirServerLogging ("Client's authentication token has expired.")
        let encPort = encryptPort sessionKey 0
        return (SecurePort encPort)
      else do
        liftIO $ logMessage dirServerLogging ("Searching for file: " ++ decFileName)
        fileMapping <- getFileMapping decFileName
        case fileMapping of
          Nothing -> do
            liftIO $ logMessage dirServerLogging ("File does not exist on servers. Picking random server to upload to...")
            port <- liftIO $ randomRIO (8081, 8083)
            let encPort = encryptPort sessionKey port
            return (SecurePort encPort)
          Just fileMapping' -> do
            let (FileMapping _ server port) = fileMapping'
            let port' = (read :: String -> Int) $ port
            liftIO $ logMessage dirServerLogging ("File found on " ++ server ++ " operating on port: " ++ port)
            let encPort = encryptPort sessionKey port'
            return (SecurePort encPort)

      where
        getFileMapping :: String -> APIHandler (Maybe FileMapping)
        getFileMapping name = do
          fileMap <- liftIO $ withMongoDbConnection $ do
            docs <- find (select ["fileName" =: name] "FILE_SERVER_MAPPINGS") >>= drainCursor
            return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileMapping) docs

          case (length fileMap) of -- Currently no replicating so the length should either be 0 or 1
            0 -> return Nothing 
            _ -> return (Just (head fileMap))

    listFiles :: SecureTicket -> APIHandler [String]
    listFiles (SecureTicket ticket encTimeOut) = do
      liftIO $ logMessage dirServerLogging ("List Files Request received.")
      let decTimeOut = decryptTime sharedServerSecret encTimeOut
      let sessionKey = encryptDecrypt sharedServerSecret ticket

      currentTime <- liftIO $ getCurrentTime
      if (currentTime > decTimeOut) then do
        liftIO $ logMessage dirServerLogging ("Client's authentication token has expired.")
        let failedArray = ["Failed", "SessionKey has timed out."]
        let encFileNames = encryptDecryptArray sessionKey failedArray
        return encFileNames
      else do
        liftIO $ logMessage dirServerLogging ("Retrieving File Mappings...")
        fileMappings <- liftIO $ withMongoDbConnection $ do
          docs <- find (select [] "FILE_SERVER_MAPPINGS") >>= drainCursor
          return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileMapping) docs
        liftIO $ logMessage dirServerLogging ("Retrieving File Names...")
        fileNames <- mapM (getFileNames) fileMappings
        liftIO $ logMessage dirServerLogging ("Sorting File Names...")
        let fileNames' = DL.sort fileNames
        let encFileNames = encryptDecryptArray sessionKey fileNames'
        return encFileNames

      where
        getFileNames :: FileMapping -> APIHandler String
        getFileNames (FileMapping fileName _ _) = return fileName

    updateLists :: String -> Int -> String -> APIHandler ResponseData
    updateLists updateType port fileName = do
      liftIO $ logMessage dirServerLogging ("Update list request received of type: " ++ updateType ++ " from server on Port " ++ show port)
      case updateType of
        "delete" -> do
          let serverNumber = port `mod` 8080
          let serverName = "Server" ++ show serverNumber
          liftIO $ logMessage dirServerLogging ("Deleting File Mapping for file: " ++ fileName)
          liftIO $ withMongoDbConnection $ do
            delete (select ["fileName" =: fileName, "serverName" =: serverName] "FILE_SERVER_MAPPINGS")
          return (ResponseData "Success")
        "update" -> do
          let serverNumber = port `mod` 8080
          let serverName = "Server" ++ show serverNumber
          let serverPort = show port 
          let fileMapping = (FileMapping fileName serverName serverPort)
          liftIO $ logMessage dirServerLogging ("Updating File Mapping for file: " ++ fileName)
          liftIO $ withMongoDbConnection $ upsert (select ["fileName" =: fileName, "serverName" =: serverName] "FILE_SERVER_MAPPINGS") $ toBSON fileMapping
          return (ResponseData "Success")
        _ -> do
          liftIO $ logMessage dirServerLogging ("Update Type: " ++ updateType ++ " does not match delete/update")
          return (ResponseData "Failure")
      

-- | File Server

uploadFile :: SecureFileUpload -> SC.ClientM SecureResponseData
deleteFile :: SecureFileName -> SC.ClientM SecureResponseData
getFiles :: SC.ClientM [String]
downloadFile :: SecureFileName -> SC.ClientM SecureFile
getModifyTime :: SecureFileName -> SC.ClientM SecureTime

fileserverApi :: Proxy FileServerAPI
fileserverApi = Proxy

uploadFile :<|> deleteFile :<|> getFiles :<|> downloadFile :<|> getModifyTime = SC.client fileserverApi

getFilesQuery :: SC.ClientM([String])
getFilesQuery = do
  get_files <- getFiles
  return (get_files)

runGetFilesQuery :: Int -> IO (Maybe [String])
runGetFilesQuery port = do
  manager <- newManager defaultManagerSettings
  res <- SC.runClientM getFilesQuery (SC.ClientEnv manager (SC.BaseUrl SC.Http fsHost port ""))
  case res of
    Left err -> do
      liftIO $ logMessage dirServerLogging ("Error in runGetFilesQuery: " ++ show err)
      return Nothing
    Right (get_files) -> do
      return (Just get_files)
