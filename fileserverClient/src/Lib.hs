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

uploadFile :: Message -> ClientM Bool
downloadFile :: Maybe String -> ClientM [Message]

api :: Proxy API
api = Proxy

(uploadFile :<|> downloadFile) = client api

queries :: ClientM (Bool, [Message])
queries = do
	upload_file <- uploadFile (Message "File1" "This is file1")
	download_file <- downloadFile (Just "File1")
	return (upload_file, download_file)

startApp :: IO ()
startApp = do
	putStrLn "Starting Client..."
	manager <- newManager defaultManagerSettings
	res <- runClientM queries (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
	case res of
		Left err -> putStrLn $ "Error: " ++ show err
		Right (uploadFile, downloadFile) -> do
			print uploadFile
			print downloadFile