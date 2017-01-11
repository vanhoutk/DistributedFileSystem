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


-- | File Server API

data File = File  { name :: String
                  , contents :: String
                  } deriving (Show, Generic, FromJSON, ToJSON)

data SecureFileUpload = SecureFileUpload { ticket :: String
                                         , encTimeout :: String
                                         , encryptedFile :: File -- Encrypted File
                                         } deriving (Show, Generic, FromJSON, ToJSON)

data SecureFileName = SecureFileName { ticket' :: String
                                     , encTimeout' :: String
                                     , encFileName :: String
                                     } deriving (Show, Generic, FromJSON, ToJSON)

data SecureFile = SecureFile { encFile :: File 
                             } deriving (Show, Generic, FromJSON, ToJSON)

data ResponseData = ResponseData { response :: String
                                 } deriving (Generic, ToJSON, FromJSON, Show)

data SecureResponseData = SecureResponseData { encResponse :: String -- Encrypted Response
                                             } deriving (Generic, ToJSON, FromJSON, Show)

data SecureTime = SecureTime { encTime :: String
                             } deriving (Generic, ToJSON, FromJSON, Show)

data SecurePort = SecurePort { encPort :: String
                             } deriving (Generic, ToJSON, FromJSON, Show)

data SecureTicket = SecureTicket { secTicket :: String
                                 , secTimeOut :: String
                                 } deriving (Generic, ToJSON, FromJSON, Show)

-- Could allow directory change
-- Could allow files to be moved
-- Could allow files to be deleted
-- Could allow directories to be created
type FileServerAPI = "upload"     :> ReqBody '[JSON] SecureFileUpload :> Post '[JSON] SecureResponseData
                :<|> "delete"     :> ReqBody '[JSON] SecureFileName   :> Post '[JSON] SecureResponseData
                :<|> "download"   :> Get '[JSON] [String] --Doesn't need to be encrypted as only called from Directory Server
                :<|> "download"   :> ReqBody '[JSON] SecureFileName   :> Post '[JSON] SecureFile
                :<|> "modifyTime" :> ReqBody '[JSON] SecureFileName   :> Post '[JSON] SecureTime

data FileMapping = FileMapping { fileName :: String
                               , serverName :: String
                               , serverPort :: String
                               } deriving (Show, Generic, FromJSON, ToJSON, FromBSON, ToBSON)

deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String

--type FileMapping = (String, String, Int)
directoryPort :: Int
directoryPort = 8080

type DirectoryServerAPI = "search"      :> ReqBody '[JSON] SecureFileName :> Post '[JSON] SecurePort
                     :<|> "list"        :> ReqBody '[JSON] SecureTicket :> Post '[JSON] [String]
                     :<|> "updateList"  :> Capture "updateType" String :> Capture "port" Int :> Capture "name" String :> Get '[JSON] ResponseData

asPort :: Int
asPort = 8090

data AccountData = AccountData { username :: String
                               , password :: String
                               } deriving (Show, Generic, FromJSON, ToJSON, FromBSON, ToBSON)

data LoginRequest = LoginRequest { userName :: String
                                 , encMessage :: String -- Encrypted Username
                                 } deriving (Show, Generic, FromJSON, ToJSON)

data AuthToken = AuthToken { encTicket :: String -- Client password encrypted ticket, which is itself a server password encrypted session key
                           , encSessionKey :: String -- Client password encrypted sessionKey
                           , encTokenTimeout :: String -- Token Timeout encryted with server password
                           } deriving (Show, Generic, FromJSON, ToJSON)

sharedServerSecret :: String
sharedServerSecret = "This is the shared server secret."

encryptDecrypt :: String -> String -> String
encryptDecrypt key text = zipWith (\a b -> chr $ xor (ord a) (ord b)) (cycle key) text
-- XOR each element of the text with a corresponding element of the key

encryptTime :: String  -> UTCTime  -> String
encryptTime key time = encryptDecrypt key (show(time) :: String)

decryptTime :: String  -> String  -> UTCTime
decryptTime key text = (read $ encryptDecrypt key text) :: UTCTime

encryptPort :: String  -> Int  -> String
encryptPort key port = encryptDecrypt key (show(port) :: String)

decryptPort :: String  -> String  -> Int
decryptPort key text = (read $ encryptDecrypt key text) :: Int

encryptDecryptArray :: String -> [String] -> [String]
encryptDecryptArray key array = do
  encryptedArray <- map (encryptDecrypt key) array
  return encryptedArray

type AuthenticationServerAPI = "login"      :> ReqBody '[JSON] LoginRequest :> Post '[JSON] AuthToken
                          :<|> "addNewUser" :> Capture "username" String    :> Capture "password" String :> Get '[JSON] ResponseData

lsPort :: Int
lsPort = 8091

data Lock = Lock { lockFileName :: String
                 , lockStatus :: Bool
                 } deriving (Show, Generic, FromJSON, ToJSON, FromBSON, ToBSON)

deriving instance FromBSON Bool  -- we need these as BSON does not provide
deriving instance ToBSON   Bool

type LockServerAPI = "lock"      :> ReqBody '[JSON] SecureFileName :> Post '[JSON] SecureResponseData
                :<|> "unlock"    :> ReqBody '[JSON] SecureFileName :> Post '[JSON] SecureResponseData
                :<|> "checkLock" :> ReqBody '[JSON] SecureFileName :> Post '[JSON] Bool