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
import 						Network.Wai
import 						Network.Wai.Handler.Warp
import 						Servant
import            System.Directory
import						APIs

type APIHandler = ExceptT ServantErr IO

startServer :: Int -> IO ()
startServer port = do
  createDirectoryIfMissing True ("files" ++ show port ++ "/")
  putStrLn "Changing current directory..."
  setCurrentDirectory ("files" ++ show port ++ "/")
  putStrLn "Starting app..."
  run port app

app :: Application
app = serve api server

api :: Proxy FileServerAPI
api = Proxy

server :: Server FileServerAPI
server = uploadFile
    :<|> getFiles
		:<|> downloadFile
    :<|> getModifyTime

	where

    uploadFile :: File -> APIHandler ResponseData
    uploadFile (File name contents) = do
      liftIO $ do
        putStrLn $ "Uploading file: " ++ name
        (writeFile name contents)
      return (ResponseData "Success")

    getFiles :: APIHandler [String]
    getFiles = liftIO $ do
      currentDirectory <- getCurrentDirectory
      putStrLn $ "Listing files in directory " ++ currentDirectory
      (listDirectory(currentDirectory))

    downloadFile :: String -> APIHandler File
    downloadFile name = do
      contents <- liftIO $ do
        putStrLn $ "Reading contents of: " ++ name
        (readFile name)
      return (File name contents)

    getModifyTime :: String -> APIHandler UTCTime
    getModifyTime name = do
      time <- liftIO $ getModificationTime name
      --liftIO $ do putStrLn $ "Modification time of " ++ name ++ ": " ++ show time
      return (time)