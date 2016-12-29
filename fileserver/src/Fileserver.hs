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
  createDirectoryIfMissing True ("files" ++ show port ++ "/")
  putStrLn "Changing current directory..."
  setCurrentDirectory ("files" ++ show port ++ "/")
  putStrLn "Starting app..."
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
    uploadFile (SecureFileUpload ticket (File encName encContents)) = do
      let sessionKey = encryptDecrypt sharedServerSecret ticket
      let decName = encryptDecrypt sessionKey encName
      let decContents = encryptDecrypt sessionKey encContents
      liftIO $ do
        putStrLn $ "Uploading file: " ++ decName
        updateListQuery "update" port decName
        (writeFile decName decContents)
      let encResponse = encryptDecrypt sessionKey "Success"
      return (SecureResponseData encResponse)

    deleteFile :: SecureFileName -> APIHandler SecureResponseData
    deleteFile (SecureFileName ticket encName) = do
      let sessionKey = encryptDecrypt sharedServerSecret ticket
      let decName = encryptDecrypt sessionKey encName
      liftIO $ do
        putStrLn $ "Deleting file: " ++ decName
        (removeFile decName)
      liftIO $ updateListQuery "delete" port decName
      let encResponse = encryptDecrypt sessionKey "Success"
      return (SecureResponseData encResponse)

    getFiles :: APIHandler [String]
    getFiles = do
      files <- liftIO $ do
        currentDirectory <- getCurrentDirectory
        putStrLn $ "Listing files in directory " ++ currentDirectory
        (listDirectory(currentDirectory))
      let files' = DL.sort files
      return files' 

    downloadFile :: SecureFileName -> APIHandler SecureFile
    downloadFile (SecureFileName ticket encName) = do
      let sessionKey = encryptDecrypt sharedServerSecret ticket
      let decName = encryptDecrypt sessionKey encName
      contents <- liftIO $ do
        putStrLn $ "Reading contents of: " ++ decName
        (readFile decName)
      let encContents = encryptDecrypt sessionKey contents
      return (SecureFile (File encName encContents))

    getModifyTime :: SecureFileName -> APIHandler UTCTime
    getModifyTime (SecureFileName ticket encName) = do
      let sessionKey = encryptDecrypt sharedServerSecret ticket
      let decName = encryptDecrypt sessionKey encName
      time <- liftIO $ getModificationTime decName
      --liftIO $ do putStrLn $ "Modification time of " ++ name ++ ": " ++ show time
      return (time)

-- | Directry Server Stuff

searchForFile :: SecureFileName -> SC.ClientM Int
getFileList :: SC.ClientM [String]
updateList :: String -> Int -> String -> SC.ClientM ResponseData

directoryServerApi :: Proxy DirectoryServerAPI
directoryServerApi = Proxy

searchForFile :<|> getFileList :<|> updateList = SC.client directoryServerApi

updateListQuery :: String -> Int -> String -> IO()
updateListQuery updateType port fileName = do
  manager <- newManager defaultManagerSettings
  res <- SC.runClientM (updateList updateType port fileName) (SC.ClientEnv manager (SC.BaseUrl SC.Http "localhost" 8080 ""))
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return ()
    Right (response) -> do
      print response
      return ()