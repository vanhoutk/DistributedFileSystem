{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module APIs where

import            Data.Aeson
import            Data.Aeson.TH
import            Data.Bits
import            Data.Bson.Generic
import            Data.Char
import            Data.Time
import            GHC.Generics
import            Servant

data File = File  { name :: String
                  , contents :: String
                  } deriving (Show, Generic, FromJSON, ToJSON)


data ResponseData = ResponseData { response :: String
                                 } deriving (Generic, ToJSON, FromJSON, Show)

-- Could allow directory change
-- Could allow files to be moved
-- Could allow files to be deleted
-- Could allow directories to be created
type FileServerAPI = "upload"     :> ReqBody '[JSON] File  :> Post '[JSON] ResponseData
                :<|> "delete"     :> Capture "name" String :> Get '[JSON] ResponseData
                :<|> "download"   :> Get '[JSON] [String]
                :<|> "download"   :> Capture "name" String :> Get '[JSON] File
                :<|> "modifyTime" :> Capture "name" String :> Get '[JSON] UTCTime

data FileMapping = FileMapping { fileName :: String
                               , serverName :: String
                               , serverPort :: String
                               } deriving (Show, Generic, FromJSON, ToJSON, FromBSON, ToBSON)

deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String

--type FileMapping = (String, String, Int)
directoryPort :: Int
directoryPort = 8080

type DirectoryServerAPI = "search"      :> Capture "name" String       :> Get '[JSON] Int
                     :<|> "list"        :> Get '[JSON] [String]
                     :<|> "updateList"  :> Capture "updateType" String :> Capture "port" Int :> Capture "name" String :> Get '[JSON] ResponseData

asPort :: Int
asPort = 8090

data AccountData = AccountData { username :: String
                               , password :: String
                               } deriving (Show, Generic, FromJSON, ToJSON, FromBSON, ToBSON)

data LoginRequest = LoginRequest { userName :: String
                                 , encMessage :: String -- Encrypted Username
                                 } deriving (Show, Generic, FromJSON, ToJSON)

encryptDecrypt :: String -> String -> String
encryptDecrypt key text = zipWith (\a b -> chr $ xor (ord a) (ord b)) (cycle key) text
-- XOR each element of the text with a corresponding element of the key

type AuthenticationServerAPI = "login"      :> ReqBody '[JSON] LoginRequest :> Post '[JSON] ResponseData
                          :<|> "addNewUser" :> Capture "username" String    :> Capture "password" String :> Get '[JSON] ResponseData