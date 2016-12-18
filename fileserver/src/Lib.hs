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

module Lib
    ( startApp
    ) where

import           	Control.Monad.IO.Class
import            Control.Monad.Trans.Except
import           	Control.Monad.Trans.Resource
import 						Data.Aeson
import						Data.Aeson.TH
import qualified	Data.List                    as DL
import           	Data.Text                    (pack, unpack)
import 						Network.Wai
import 						Network.Wai.Handler.Warp
import 						Servant
import            System.Directory
import						FileserverAPI

type APIHandler = ExceptT ServantErr IO

startApp :: IO ()
startApp = do
  createDirectoryIfMissing True ("files/")
  putStrLn "Changing current directory..."
  setCurrentDirectory ("files/")
  putStrLn "Starting app..."
  run 8080 app

app :: Application
app = serve api server

api :: Proxy FileServerAPI
api = Proxy

server :: Server FileServerAPI
server = uploadFile
    :<|> getFiles
		:<|> downloadFile

	where

    uploadFile :: File -> APIHandler ResponseData
    uploadFile (File name contents) = do
      liftIO $ do
        putStrLn $ "Uploading file: " ++ name
        (writeFile name contents)
      return (ResponseData "Success")

    getFiles :: APIHandler [String]
    getFiles = liftIO $ do
      putStrLn "Listing files in directory ../files/"
      (listDirectory("../files/"))

    downloadFile :: String -> APIHandler File
    downloadFile name = do
      contents <- liftIO $ do
        putStrLn $ "Reading contents of: " ++ name
        (readFile name)
      return (File name contents)