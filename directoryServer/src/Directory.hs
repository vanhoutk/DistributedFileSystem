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
import qualified  Data.List                     as DL
import            Data.Maybe
import            Data.Proxy
import            Data.Text                     (pack, unpack)
import            Data.Time
import            Network.HTTP.Client           (newManager, defaultManagerSettings)
import            Network.Wai
import            Network.Wai.Handler.Warp
import            Servant
import qualified  Servant.API                   as SC
import qualified  Servant.Client                as SC
import            System.Directory
import            APIs

type APIHandler = ExceptT ServantErr IO

fileServerPorts :: [Int]
fileServerPorts = [8081, 8082]

startDirectory :: IO ()
startDirectory = do
  createDirectoryIfMissing True ("fileservers/")
  putStrLn "Changing current directory..."
  setCurrentDirectory ("fileservers/")
  putStrLn "Starting app..."
  fileMappings <- initDirectory fileServerPorts
  run 8080 app

initDirectory :: [Int] -> IO()
initDirectory ports = do
  fileMappingList <- getFileMappingList ports
  writeFile "fileMappingList" (fileMappingList)
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
      return $ (FileMapping fileName serverName port):a

app :: Application
app = serve api server

api :: Proxy DirectoryServerAPI
api = Proxy

server :: Server DirectoryServerAPI
server = searchForFile
    :<|> listFiles
    :<|> updateLists

  where

    searchForFile :: String -> APIHandler Int
    searchForFile fileName = do
      fileMappingList <- readFile "fileMappingList"
      print fileMappingList
      let returnValue = 1
      return returnValue
      {-liftIO $ do
        putStrLn $ "Uploading file: " ++ name
        (writeFile name contents)
      return (ResponseData "Success")-}

    listFiles :: APIHandler [String]
    listFiles = do
      return []
      {-putStrLn "Listing files in directory ../files/"
      (listDirectory("../files/"))-}

    updateLists :: [String] -> Int -> APIHandler ResponseData
    updateLists files port = do
      {-contents <- liftIO $ do
        putStrLn $ "Reading contents of: " ++ name
        (readFile name)-}
      return (ResponseData "Success")

-- | File Server

uploadFile :: File -> SC.ClientM ResponseData
getFiles :: SC.ClientM [String]
downloadFile :: String -> SC.ClientM File
getModifyTime :: String -> SC.ClientM UTCTime

fileserverApi :: Proxy FileServerAPI
fileserverApi = Proxy

uploadFile :<|> getFiles :<|> downloadFile :<|> getModifyTime = SC.client fileserverApi

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
