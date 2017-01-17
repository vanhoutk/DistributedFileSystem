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

module TS
    ( startTransactionServer
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
import            Servant.API               
import            Servant.Client                
import            System.Directory
import            System.Random

import            APIs
import            MongoFunctions

type APIHandler = ExceptT ServantErr IO

startTransactionServer :: IO ()
startTransactionServer = do
  logMessage tranServerLogging ("Starting Transaction Server...")
  run tsPort app
  
app :: Application
app = serve api server

api :: Proxy TransactionServerAPI
api = Proxy

server :: Server TransactionServerAPI
server = startTransaction 
    :<|> downloadTransaction
    :<|> cachedTransaction
    :<|> uploadTransaction
    :<|> commitTransaction
    :<|> abortTransaction

    where

      startTransaction :: SecureTicket -> APIHandler SecureResponseData
      startTransaction (SecureTicket ticket encTimeOut) = do
        let decTimeOut = decryptTime sharedServerSecret encTimeOut
        let sessionKey = encryptDecrypt sharedServerSecret ticket

        liftIO $ logMessage tranServerLogging ("Starting New Transaction. Transaction ID: " ++ sessionKey)

        currentTime <- liftIO $ getCurrentTime
        if (currentTime > decTimeOut) then do
          liftIO $ logMessage tranServerLogging ("Client's authentication token has expired.")
          let encResponse = encryptDecrypt sessionKey "Failed - SessionKey has expired."
          return (SecureResponseData encResponse)
        else do
          liftIO $ logMessage tranServerLogging ("Storing Transaction ID...")
          liftIO $ withMongoDbConnection $ upsert (select ["transactionID" =: sessionKey] "TRANSACTIONS") $ toBSON sessionKey
          let encResponse = encryptDecrypt sessionKey "Success - New Transaction Started."
          return (SecureResponseData encResponse)

      downloadTransaction :: SecureFileName -> APIHandler SecureFile
      downloadTransaction secFileName@(SecureFileName ticket encTimeOut encFileName) = do
        let decTimeOut = decryptTime sharedServerSecret encTimeOut
        let sessionKey = encryptDecrypt sharedServerSecret ticket
        let decFileName = encryptDecrypt sessionKey encFileName

        liftIO $ logMessage tranServerLogging ("Download Transaction: Transaction ID: " ++ sessionKey ++ " File: " ++ decFileName)

        currentTime <- liftIO $ getCurrentTime
        if (currentTime > decTimeOut) then do
          liftIO $ logMessage tranServerLogging ("Client's authentication token has expired.")
          liftIO $ withMongoDbConnection $ delete (select ["transactionID" =: sessionKey] "TRANSACTIONS")
          abortT ticket encTimeOut sessionKey
          return (SecureFile (File "Failed" "Session Key has expired. Transaction Aborted"))
        else do
          -- Search for all servers that contain the file so that we can replicate to all of them if needs be
          manager <- liftIO $ newManager defaultManagerSettings
          res <- liftIO $ runClientM (searchManyQ decFileName) (ClientEnv manager (BaseUrl Http dsHost dsPort ""))
          case res of
            Left err -> do
              liftIO $ logMessage tranServerLogging ("Error getting fileserver ports from Directory Server: " ++ show err)
              return (SecureFile (File "Failed" ("Error getting fileserver ports from Directory Server: " ++ show err)))
            Right (ports) -> do
              -- Add the list of servers to the database
              mapM (addNewMaps sessionKey decFileName) ports
              -- Download the file from one of the servers (assumes replication is synchronous so all copies should be the same)
              resDownload <- liftIO $ runClientM (downloadF secFileName) (ClientEnv manager (BaseUrl Http dsHost (head ports) ""))
              case resDownload of
                Left errDownload -> do
                  liftIO $ logMessage tranServerLogging ("Error download file: " ++ show errDownload)
                  return (SecureFile (File "Failed" ("Error download file: " ++ show errDownload)))
                Right (file) -> do
                  return file
      
      -- Called when the user accesses a while which is already cached, but the Transaction Server still needs a log of it
      cachedTransaction :: SecureFileName -> APIHandler SecureResponseData
      cachedTransaction (SecureFileName ticket encTimeOut encFileName) = do
        let decTimeOut = decryptTime sharedServerSecret encTimeOut
        let sessionKey = encryptDecrypt sharedServerSecret ticket
        let decFileName = encryptDecrypt sessionKey encFileName

        liftIO $ logMessage tranServerLogging ("Cached Transaction: Transaction ID: " ++ sessionKey ++ " File: " ++ decFileName)

        currentTime <- liftIO $ getCurrentTime
        if (currentTime > decTimeOut) then do
          liftIO $ logMessage tranServerLogging ("Client's authentication token has expired.")
          liftIO $ withMongoDbConnection $ delete (select ["transactionID" =: sessionKey] "TRANSACTIONS")
          abortT ticket encTimeOut sessionKey
          let encResponse = encryptDecrypt sessionKey "Failed - SessionKey has expired. Transaction Aborted."
          return (SecureResponseData encResponse)
        else do
          liftIO $ logMessage tranServerLogging ("Checking if transaction map already exists...")
          transactionMaps <- liftIO $ withMongoDbConnection $ do
            docs <- find (select ["userid" =: sessionKey, "servFileName" =: decFileName] "TRANSACTION_MAPPINGS") >>= drainCursor
            return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe TransactionData) docs

          case (length transactionMaps) of 
            0 -> do 
              manager <- liftIO $ newManager defaultManagerSettings
              res <- liftIO $ runClientM (searchManyQ decFileName) (ClientEnv manager (BaseUrl Http dsHost dsPort ""))
              case res of
                Left err -> do
                  liftIO $ logMessage tranServerLogging ("Error getting fileserver ports from Directory Server: " ++ show err)
                  let encResponse = encryptDecrypt sessionKey "Failed - Error getting fileserver ports from Directory Server."
                  return (SecureResponseData encResponse)
                Right (ports) -> do
                  mapM (addNewMaps sessionKey decFileName) ports
                  liftIO $ logMessage tranServerLogging ("New Transactions map added to database.")
                  let encResponse = encryptDecrypt sessionKey "Success - Added new transaction maps."
                  return (SecureResponseData encResponse)
            _ -> do
              liftIO $ logMessage tranServerLogging ("Transactions map already exist.")
              let encResponse = encryptDecrypt sessionKey "Success - Transaction Map already exists."
              return (SecureResponseData encResponse)

      uploadTransaction :: SecureFileUpload -> APIHandler SecureResponseData
      uploadTransaction file@(SecureFileUpload ticket encTimeOut (File encFileName encContents)) = do
        let decTimeOut = decryptTime sharedServerSecret encTimeOut
        let sessionKey = encryptDecrypt sharedServerSecret ticket
        let decFileName = encryptDecrypt sessionKey encFileName

        liftIO $ logMessage tranServerLogging ("Upload Transaction: Transaction ID: " ++ sessionKey ++ " File: " ++ decFileName)

        currentTime <- liftIO $ getCurrentTime
        if (currentTime > decTimeOut) then do
          liftIO $ logMessage tranServerLogging ("Client's authentication token has expired.")
          liftIO $ withMongoDbConnection $ delete (select ["transactionID" =: sessionKey] "TRANSACTIONS")
          abortT ticket encTimeOut sessionKey
          let encResponse = encryptDecrypt sessionKey "Failed - SessionKey has expired. Transaction Aborted."
          return (SecureResponseData encResponse)
        else do
          liftIO $ logMessage tranServerLogging ("Searching for transaction mapping...") 
          uploadResult <- uploadT file sessionKey decFileName
          case uploadResult of
            False -> do
              let encResponse = encryptDecrypt sessionKey "Failed - Unable to upload a new file during a transaction."
              return (SecureResponseData encResponse)
            True -> do
              let encResponse = encryptDecrypt sessionKey "Success - Upload completed."
              return (SecureResponseData encResponse)

        where
          uploadT :: SecureFileUpload -> String -> String -> APIHandler (Bool)
          uploadT file@(SecureFileUpload ticket encTimeOut (File encFileName encContents)) transactionID fileName = do
            transactionMaps <- liftIO $ withMongoDbConnection $ do
              docs <- find (select ["userid" =: transactionID, "servFileName" =: fileName] "TRANSACTION_MAPPINGS") >>= drainCursor
              return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe TransactionData) docs

            case (length transactionMaps) of 
              0 -> do
                liftIO $ logMessage tranServerLogging ("Transaction map does not exist.")
                return False
              _ -> do
                liftIO $ logMessage tranServerLogging ("Transaction map exists.")
                let tempFileName = fileName ++ "~"
                liftIO $ logMessage tranServerLogging ("Uploading temp file: " ++ tempFileName ++ " to relevant servers...") 
                let encTempFileName = encryptDecrypt transactionID tempFileName
                mapM (uploadEachT (SecureFileUpload ticket encTimeOut (File encTempFileName encContents))) transactionMaps
                mapM (updateMaps fileName tempFileName) transactionMaps
                return True

          uploadEachT :: SecureFileUpload -> TransactionData -> APIHandler ()
          uploadEachT file (TransactionData userid _ _ serverPort) = do
            manager <- liftIO $ newManager defaultManagerSettings
            res <- liftIO $ runClientM (uploadF file) (ClientEnv manager (BaseUrl Http fsHost (read serverPort :: Int) ""))
            case res of
              Left err -> do
                liftIO $ logMessage tranServerLogging ("Error uploading file: " ++ show err)
                return ()
              Right (SecureResponseData encResponse) -> do
                let decResponse = encryptDecrypt userid encResponse
                liftIO $ logMessage tranServerLogging ("Upload File Response: " ++ decResponse)
                return ()

          updateMaps :: String -> String -> TransactionData -> APIHandler ()
          updateMaps fileName tempFileName (TransactionData userid _ _ serverPort) = do
            let transactionMap = (TransactionData userid fileName tempFileName serverPort)
            liftIO $ withMongoDbConnection $ do
              upsert (select ["userid" =: userid, "servFileName" =: fileName, "serverPort" =: serverPort] "TRANSACTION_MAPPINGS") $ toBSON transactionMap 
            return ()


      commitTransaction :: SecureTicket -> APIHandler SecureResponseData
      commitTransaction (SecureTicket ticket encTimeOut) = do
        let decTimeOut = decryptTime sharedServerSecret encTimeOut
        let sessionKey = encryptDecrypt sharedServerSecret ticket

        liftIO $ logMessage tranServerLogging ("Committing Transaction. Transaction ID: " ++ sessionKey)

        currentTime <- liftIO $ getCurrentTime
        if (currentTime > decTimeOut) then do
          liftIO $ logMessage tranServerLogging ("Client's authentication token has expired.")
          liftIO $ withMongoDbConnection $ delete (select ["transactionID" =: sessionKey] "TRANSACTIONS")
          abortT ticket encTimeOut sessionKey
          let encResponse = encryptDecrypt sessionKey "Failed - SessionKey has expired. Transaction Aborted."
          return (SecureResponseData encResponse)
        else do
          commitT ticket encTimeOut sessionKey
          liftIO $ withMongoDbConnection $ delete (select ["transactionID" =: sessionKey] "TRANSACTIONS")
          liftIO $ logMessage tranServerLogging ("Transaction successfully committed.")
          let encResponse = encryptDecrypt sessionKey "Success - Transaction Committed."
          return (SecureResponseData encResponse)

      abortTransaction :: SecureTicket -> APIHandler SecureResponseData
      abortTransaction (SecureTicket ticket encTimeOut) = do
        let sessionKey = encryptDecrypt sharedServerSecret ticket

        liftIO $ logMessage tranServerLogging ("Aborting Transaction. Transaction ID: " ++ sessionKey)

        liftIO $ logMessage tranServerLogging ("Deleting Transaction...")
        liftIO $ withMongoDbConnection $ delete (select ["transactionID" =: sessionKey] "TRANSACTIONS")
        abortT ticket encTimeOut sessionKey
        let encResponse = encryptDecrypt sessionKey "Success - Transaction Aborted."
        return (SecureResponseData encResponse)



commitT :: String -> String -> String -> APIHandler ()
commitT ticket encTimeOut transactionID = do
  transactionMaps <- liftIO $ withMongoDbConnection $ do
    docs <- find (select ["userid" =: transactionID] "TRANSACTION_MAPPINGS") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe TransactionData) docs

  case (length transactionMaps) of 
    0 -> return ()
    _ -> do
      liftIO $ logMessage tranServerLogging ("Committing transaciton on each server...")
      mapM (commitEachT) transactionMaps
      liftIO $ logMessage tranServerLogging ("Unlocking all held locks...")
      mapM (unlockAllFiles ticket encTimeOut) transactionMaps
      liftIO $ withMongoDbConnection $ delete (select ["userid" =: transactionID] "TRANSACTION_MAPPINGS")
      return ()

  where
    commitEachT :: TransactionData -> APIHandler ()
    commitEachT (TransactionData userid servFileName tempFileName serverPort) = do
      if (servFileName == tempFileName) then return ()
      else do
        manager <- liftIO $ newManager defaultManagerSettings
        res <- liftIO $ runClientM (commitF servFileName tempFileName) (ClientEnv manager (BaseUrl Http fsHost (read serverPort :: Int) ""))
        case res of
          Left err -> do
            liftIO $ logMessage tranServerLogging ("Error committing file: " ++ show err)
            return ()
          Right (ResponseData response) -> do
            liftIO $ logMessage tranServerLogging ("Commit File Response: " ++ response)
            return ()


abortT :: String -> String -> String -> APIHandler ()
abortT ticket encTimeOut transactionID = do
  transactionMaps <- liftIO $ withMongoDbConnection $ do
    docs <- find (select ["userid" =: transactionID] "TRANSACTION_MAPPINGS") >>= drainCursor
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe TransactionData) docs

  case (length transactionMaps) of 
    0 -> return ()
    _ -> do
      liftIO $ logMessage tranServerLogging ("Aborting transaction on each server...")
      mapM (abortEachT ticket encTimeOut) transactionMaps
      liftIO $ logMessage tranServerLogging ("Unlocking all held locks...")
      mapM (unlockAllFiles ticket encTimeOut) transactionMaps
      liftIO $ withMongoDbConnection $ do
        delete (select ["userid" =: transactionID] "TRANSACTION_MAPPINGS")
      return ()

  where
    abortEachT :: String -> String -> TransactionData -> APIHandler ()
    abortEachT ticket encTimeOut (TransactionData userid servFileName tempFileName serverPort) = do
      if (servFileName == tempFileName) then return ()
      else do
        let encFileName = encryptDecrypt userid tempFileName
        manager <- liftIO $ newManager defaultManagerSettings
        res <- liftIO $ runClientM (deleteF ticket encTimeOut encFileName) (ClientEnv manager (BaseUrl Http fsHost (read serverPort :: Int) ""))
        case res of
          Left err -> do
            liftIO $ logMessage tranServerLogging ("Error deleting file: " ++ show err)
            return ()
          Right (SecureResponseData encResponse) -> do
            let decResponse = encryptDecrypt userid encResponse
            liftIO $ logMessage tranServerLogging ("Delete File Response: " ++ decResponse)
            return ()

addNewMaps :: String -> String -> Int -> APIHandler ()
addNewMaps userid fileName port = do
  let transactionMap = (TransactionData userid fileName fileName (show port))
  liftIO $ withMongoDbConnection $ do
    upsert (select ["userid" =: userid, "servFileName" =: fileName, "serverPort" =: port] "TRANSACTION_MAPPINGS") $ toBSON transactionMap 
  return ()
        

-- | File Server

uploadFile :: SecureFileUpload -> ClientM SecureResponseData
deleteFile :: SecureFileName -> ClientM SecureResponseData
getFiles :: ClientM [String]
downloadFile :: SecureFileName -> ClientM SecureFile
getModifyTime :: SecureFileName -> ClientM SecureTime
commitFile :: String -> String -> ClientM ResponseData

fileserverApi :: Proxy FileServerAPI
fileserverApi = Proxy

uploadFile :<|> deleteFile :<|> getFiles :<|> downloadFile :<|> getModifyTime :<|> commitFile = client fileserverApi

uploadF :: SecureFileUpload -> ClientM SecureResponseData
uploadF file = do
  response <- uploadFile file
  return (response)

deleteF :: String -> String -> String -> ClientM SecureResponseData
deleteF ticket encTimeOut encFileName = do
  response <- deleteFile (SecureFileName ticket encTimeOut encFileName)
  return (response)

commitF :: String -> String -> ClientM ResponseData
commitF sysFileName tempFileName = do
  response <- commitFile sysFileName tempFileName
  return (response)

downloadF :: SecureFileName -> ClientM SecureFile
downloadF secFileName = do
  file <- downloadFile secFileName
  return (file)


-- | Directory Server Stuff

searchForFile :: SecureFileName -> ClientM SecurePort
searchForMany :: String -> ClientM [Int]
uploadToServer :: SecureFileUpload -> ClientM SecureResponseData
getFileList :: SecureTicket -> ClientM [String]
updateList :: String -> Int -> String -> ClientM ResponseData

directoryServerApi :: Proxy DirectoryServerAPI
directoryServerApi = Proxy

searchForFile :<|> searchForMany :<|> uploadToServer :<|> getFileList :<|> updateList = client directoryServerApi

searchManyQ :: String -> ClientM [Int]
searchManyQ fileName = do
  ports <- searchForMany fileName
  return ports


-- | Lock Server

lockFile :: SecureFileName -> ClientM SecureResponseData
unlockFile :: SecureFileName -> ClientM SecureResponseData
checkLockFile :: SecureFileName -> ClientM Bool

lockServerAPI :: Proxy LockServerAPI
lockServerAPI = Proxy

lockFile :<|> unlockFile :<|> checkLockFile = client lockServerAPI

unlockQuery :: String -> String -> String -> ClientM SecureResponseData
unlockQuery ticket encTimeOut fileName = do
  unlockQ <- unlockFile (SecureFileName ticket encTimeOut fileName)
  return unlockQ

unlockAllFiles :: String -> String -> TransactionData -> APIHandler ()
unlockAllFiles ticket encTimeOut (TransactionData userid servFileName tempFileName serverPort) = do
  let encFileName = encryptDecrypt userid tempFileName
  manager <- liftIO $ newManager defaultManagerSettings
  res <- liftIO $ runClientM (unlockQuery ticket encTimeOut encFileName) (ClientEnv manager (BaseUrl Http lsHost lsPort ""))
  case res of
    Left err -> do
      liftIO $ logMessage tranServerLogging ("Error unlocking file: " ++ show err)
      return ()
    Right (SecureResponseData encResponse) -> do
      let decResponse = encryptDecrypt userid encResponse
      liftIO $ logMessage tranServerLogging ("Response from Lock Server: " ++ decResponse)
      return ()