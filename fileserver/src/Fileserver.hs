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

module Fileserver
    ( startServer
    ) where

import           	Control.Monad.IO.Class
import            Control.Monad.Trans.Except
import           	Control.Monad.Trans.Resource
import 						Data.Aeson
import						Data.Aeson.TH
import qualified	Data.List                    as DL
import           	Data.Text                    (pack, unpack)
import            Data.Time
import            Network.HTTP.Client          (newManager, defaultManagerSettings)
import 						Network.Wai
import 						Network.Wai.Handler.Warp
import 						Servant
import qualified  Servant.API                  as SC
import qualified  Servant.Client               as SC  
import            System.Directory

import						APIs

type APIHandler = ExceptT ServantErr IO

startServer :: Int -> IO ()
startServer port = do
  logMessage fileServerLogging ("Creating directory files" ++ show port ++ "/ ...")
  createDirectoryIfMissing True ("files" ++ show port ++ "/")
  logMessage fileServerLogging ("Changing current directory...")
  setCurrentDirectory ("files" ++ show port ++ "/")
  logMessage fileServerLogging ("Starting fileserver on port " ++ show port ++ "...")
  run port $ app port

app :: Int -> Application
app port = serve api $ server port

api :: Proxy FileServerAPI
api = Proxy

server :: Int -> Server FileServerAPI
server port = uploadFile
         :<|> deleteFile
         :<|> getFiles
		     :<|> downloadFile
         :<|> getModifyTime

	where

    uploadFile :: SecureFileUpload -> APIHandler SecureResponseData
    uploadFile (SecureFileUpload ticket encTimeOut (File encName encContents)) = do
      let decTimeOut = decryptTime sharedServerSecret encTimeOut
      let sessionKey = encryptDecrypt sharedServerSecret ticket
      let decName = encryptDecrypt sessionKey encName

      liftIO $ logMessage fileServerLogging ("Upload Request received for file: " ++ decName)

      currentTime <- liftIO $ getCurrentTime
      if (currentTime > decTimeOut) then do -- Token has expired
        liftIO $ logMessage fileServerLogging ("Client's authentication token has expired.")
        let encResponse = encryptDecrypt sessionKey "Failed - SessionKey has expired."
        return (SecureResponseData encResponse)
      else do -- Token still valid
        let decContents = encryptDecrypt sessionKey encContents
        liftIO $ do
          logMessage fileServerLogging ("Storing file: " ++ decName ++ " ...")
          updateListQuery "update" port decName -- Send a message to the directory server letting it know a file has been uploaded
          (writeFile decName decContents)
        let encResponse = encryptDecrypt sessionKey "Success"
        return (SecureResponseData encResponse)

    deleteFile :: SecureFileName -> APIHandler SecureResponseData
    deleteFile (SecureFileName ticket encTimeOut encName) = do
      let decTimeOut = decryptTime sharedServerSecret encTimeOut
      let sessionKey = encryptDecrypt sharedServerSecret ticket
      let decName = encryptDecrypt sessionKey encName

      liftIO $ logMessage fileServerLogging ("Delete Request received for file: " ++ decName)

      currentTime <- liftIO $ getCurrentTime
      if (currentTime > decTimeOut) then do -- Token has expired
        liftIO $ logMessage fileServerLogging ("Client's authentication token has expired.")
        let encResponse = encryptDecrypt sessionKey "Failed - SessionKey has expired."
        return (SecureResponseData encResponse)
      else do -- Token still valid
        liftIO $ do
          logMessage fileServerLogging ("Deleting file: " ++ decName ++ " ...")
          updateListQuery "delete" port decName -- Send a message to the directory server letting it know a file has been deleted
          (removeFile decName)
        let encResponse = encryptDecrypt sessionKey "Success"
        return (SecureResponseData encResponse)

    getFiles :: APIHandler [String]
    getFiles = do
      files <- liftIO $ do
        currentDirectory <- getCurrentDirectory
        logMessage fileServerLogging ("Listing Files in directory: " ++ currentDirectory ++ " ...")
        (listDirectory(currentDirectory))
      let files' = DL.sort files
      return files' 

    downloadFile :: SecureFileName -> APIHandler SecureFile
    downloadFile (SecureFileName ticket encTimeOut encName) = do
      let decTimeOut = decryptTime sharedServerSecret encTimeOut
      let sessionKey = encryptDecrypt sharedServerSecret ticket
      let decName = encryptDecrypt sessionKey encName

      liftIO $ logMessage fileServerLogging ("Download Request received for file: " ++ decName)

      currentTime <- liftIO $ getCurrentTime
      if (currentTime > decTimeOut) then do
        liftIO $ logMessage fileServerLogging ("Client's authentication token has expired.")
        let encContents = encryptDecrypt sessionKey "Failed - SessionKey has expired."
        return (SecureFile (File encName encContents))
      else do
        contents <- liftIO $ do
          logMessage fileServerLogging ("Reading contents of: " ++ decName)
          (readFile decName)
        let encContents = encryptDecrypt sessionKey contents
        return (SecureFile (File encName encContents))

    getModifyTime :: SecureFileName -> APIHandler SecureTime
    getModifyTime (SecureFileName ticket encTimeOut encName) = do
      let sessionKey = encryptDecrypt sharedServerSecret ticket
      let decName = encryptDecrypt sessionKey encName
      liftIO $ logMessage fileServerLogging ("Modification Time Request received for file: " ++ decName)
      time <- liftIO $ getModificationTime decName
      liftIO $ logMessage fileServerLogging ("Modification time of " ++ decName ++ ": " ++ show time)
      let encTime = encryptTime sessionKey time
      return (SecureTime encTime)


-- | Directory Server Stuff

searchForFile :: SecureFileName -> SC.ClientM SecurePort
uploadToServer :: SecureFileUpload -> SC.ClientM SecureResponseData
getFileList :: SecureTicket -> SC.ClientM [String]
updateList :: String -> Int -> String -> SC.ClientM ResponseData

directoryServerApi :: Proxy DirectoryServerAPI
directoryServerApi = Proxy

searchForFile :<|> uploadToServer :<|> getFileList :<|> updateList = SC.client directoryServerApi

updateListQuery :: String -> Int -> String -> IO()
updateListQuery updateType port fileName = do
  manager <- newManager defaultManagerSettings
  res <- SC.runClientM (updateList updateType port fileName) (SC.ClientEnv manager (SC.BaseUrl SC.Http host dsPort ""))
  case res of
    Left err -> do
      logMessage fileServerLogging ("Error updating directory list: " ++ show err)
      return ()
    Right (ResponseData response) -> do
      logMessage fileServerLogging ("Response from directory updateListQuery: " ++ response)
      return ()