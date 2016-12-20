{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Directory
    ( startDirectory
    ) where

import            Control.Monad.IO.Class
import            Control.Monad.Trans.Except
import            Control.Monad.Trans.Resource
import            Data.Aeson
import            Data.Aeson.TH
import qualified  Data.List                    as DL
import            Data.Text                    (pack, unpack)
import            Network.Wai
import            Network.Wai.Handler.Warp
import            Servant
import            System.Directory
import            APIs

type APIHandler = ExceptT ServantErr IO

startDirectory :: IO ()
startApp = do
  createDirectoryIfMissing True ("fileservers/")
  putStrLn "Changing current directory..."
  setCurrentDirectory ("fileservers/")
  putStrLn "Starting app..."
  run 8080 app

app :: Application
app = serve api server

api :: Proxy DirectoryServerAPI
api = Proxy

server :: Server DirectoryServerAPI
server = searchForFile
    :<|> getFiles
    :<|> updateLists

  where

    searchForFile :: String -> APIHandler Int
    searchForFile fileName = do
      liftIO $ do
        putStrLn $ "Uploading file: " ++ name
        (writeFile name contents)
      return (ResponseData "Success")

    getFiles :: APIHandler [String]
    getFiles = liftIO $ do
      putStrLn "Listing files in directory ../files/"
      (listDirectory("../files/"))

    updateList :: String -> [String] -> APIHandler ResponseData
    downloadFile name = do
      contents <- liftIO $ do
        putStrLn $ "Reading contents of: " ++ name
        (readFile name)
      return (ResponseData "Success")


startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users