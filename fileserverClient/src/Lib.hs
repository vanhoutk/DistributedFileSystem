{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell 	   	 #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Lib
    ( startApp
    ) where

import 						Data.Aeson
import 						Data.Proxy
import 						GHC.Generics
import 						Network.HTTP.Client (newManager, defaultManagerSettings)
import						Servant.API
import						Servant.Client
import						FileserverAPI

uploadFile :: File -> ClientM ResponseData
getFiles :: ClientM [String]
downloadFile :: String -> ClientM File

api :: Proxy API
api = Proxy

uploadFile :<|> getFiles :<|> downloadFile = client api

queries :: ClientM (ResponseData, [String], File)
queries = do
	upload_file <- uploadFile (File "File1.txt" "This is file1")
	get_files <- getFiles
	download_file <- downloadFile ("File2.txt")
	return (upload_file, get_files, download_file)

startApp :: IO ()
startApp = do
	putStrLn "Starting Client..."
	manager <- newManager defaultManagerSettings
	res <- runClientM queries (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
	case res of
		Left err -> putStrLn $ "Error: " ++ show err
		Right (upload_file, get_files, download_file) -> do
			print upload_file
			print get_files
			print download_file