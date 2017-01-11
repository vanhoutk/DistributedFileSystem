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
import            APIs
import            MongoFunctions

type APIHandler = ExceptT ServantErr IO

fileServerPorts :: [Int]
fileServerPorts = [8081, 8082]

startDirectory :: IO ()
startDirectory = do
  initDirectory fileServerPorts
  run 8080 app

initDirectory :: [Int] -> IO()
initDirectory ports = do
  fileMappingList <- getFileMappingList ports
  --writeFile "fileMappingList" (fileMappingList)
  return ()

getFileMappingList :: [Int] -> IO [FileMapping]
getFileMappingList ports = do
  fileMappingList <- mapM (fileMapList []) ports
  return $ concat fileMappingList

  where
    fileMapList :: [FileMapping] -> Int -> IO [FileMapping]
    fileMapList a port = do
      files <- runGetFilesQuery port
      case files of
        Nothing -> do
          putStrLn "Error getting file list..."
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
      print fileMapping
      putStrLn $ "Filename: " ++ fileName ++ " Servername: " ++ serverName
      withMongoDbConnection $ upsert (select ["fileName" =: fileName, "serverName" =: serverName] "FILE_SERVER_MAPPINGS") $ toBSON fileMapping 
      putStrLn "After withMongoDbConnection"
      return $ (FileMapping fileName serverName (show port)):a

app :: Application
app = serve api server

api :: Proxy DirectoryServerAPI
api = Proxy

server :: Server DirectoryServerAPI
server = searchForFile
    :<|> listFiles
    :<|> updateLists

  where

    searchForFile :: SecureFileName -> APIHandler SecurePort
    searchForFile (SecureFileName ticket encTimeOut encFileName) = do
      let decTimeOut = decryptTime sharedServerSecret encTimeOut
      let sessionKey = encryptDecrypt sharedServerSecret ticket
      currentTime <- liftIO $ getCurrentTime
      liftIO $ putStrLn $ "EncFileName = " ++ encFileName
      if (currentTime > decTimeOut) then do
        let encPort = encryptPort sessionKey 0
        return (SecurePort encPort)
      else do
        let decFileName = encryptDecrypt sessionKey encFileName
        liftIO $ putStrLn $ ticket ++ " " ++ sessionKey ++ " " ++ encFileName ++ " " ++ decFileName
        fileMapping <- getFileMapping decFileName
        case fileMapping of
          Nothing -> do
            let encPort = encryptPort sessionKey 0
            return (SecurePort encPort)
          Just fileMapping' -> do
            let (FileMapping _ _ port) = fileMapping'
            let port' = (read :: String -> Int) $ port
            let encPort = encryptPort sessionKey port'
            return (SecurePort encPort)

      where
        getFileMapping :: String -> APIHandler (Maybe FileMapping)
        getFileMapping name = do
          let serverName = "Server2" :: String
          liftIO $ putStrLn $ "Filename: " ++ name ++ " Servername: " ++ serverName
          fileMap <- liftIO $ withMongoDbConnection $ do
            docs <- find (select ["fileName" =: name] "FILE_SERVER_MAPPINGS") >>= drainCursor
            liftIO $ print docs
            return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileMapping) docs

          liftIO $ print fileMap
          case (length fileMap) of
            0 -> return Nothing 
            _ -> return (Just (head fileMap))

    listFiles :: SecureTicket -> APIHandler [String]
    listFiles (SecureTicket ticket encTimeOut) = do
      liftIO $ putStrLn "ListFiles"
      let decTimeOut = decryptTime sharedServerSecret encTimeOut
      let sessionKey = encryptDecrypt sharedServerSecret ticket
      currentTime <- liftIO $ getCurrentTime
      if (currentTime > decTimeOut) then do
        let failedArray = ["Failed", "SessionKey has timed out."]
        let encFileNames = encryptDecryptArray sessionKey failedArray
        return encFileNames
      else do
        fileMappings <- liftIO $ withMongoDbConnection $ do
          docs <- find (select [] "FILE_SERVER_MAPPINGS") >>= drainCursor
          return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileMapping) docs
        fileNames <- mapM (getFileNames) fileMappings
        let fileNames' = DL.sort fileNames
        liftIO $ print fileNames'
        let encFileNames = encryptDecryptArray sessionKey fileNames'
        liftIO $ print encFileNames
        return encFileNames

      where
        getFileNames :: FileMapping -> APIHandler String
        getFileNames (FileMapping fileName _ _) = return fileName

    updateLists :: String -> Int -> String -> APIHandler ResponseData
    updateLists updateType port fileName = do
      case updateType of
        "delete" -> do
          let serverNumber = port `mod` 8080
          let serverName = "Server" ++ show serverNumber
          liftIO $ withMongoDbConnection $ do
            delete (select ["fileName" =: fileName, "serverName" =: serverName] "FILE_SERVER_MAPPINGS")
          return (ResponseData "Success")
        "update" -> do
          let serverNumber = port `mod` 8080
          let serverName = "Server" ++ show serverNumber
          let serverPort = show port 
          let fileMapping = (FileMapping fileName serverName serverPort)
          liftIO $ withMongoDbConnection $ upsert (select ["fileName" =: fileName, "serverName" =: serverName] "FILE_SERVER_MAPPINGS") $ toBSON fileMapping
          return (ResponseData "Success")
        _ -> do
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
  res <- SC.runClientM getFilesQuery (SC.ClientEnv manager (SC.BaseUrl SC.Http "localhost" port ""))
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return Nothing
    Right (get_files) -> do
      return (Just get_files)
