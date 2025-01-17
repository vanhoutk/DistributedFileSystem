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


-- | Logging Variables

fileServerLogging :: Bool
fileServerLogging = True

authServerLogging :: Bool
authServerLogging = True

dirServerLogging :: Bool
dirServerLogging = True

lockServerLogging :: Bool
lockServerLogging = True

tranServerLogging :: Bool
tranServerLogging = True

clientLogging :: Bool
clientLogging = True

cacheLogging :: Bool
cacheLogging = True

logMessage :: Bool -> String -> IO()
logMessage logBool message = do
  if(logBool) then putStrLn message
  else return ()


-- | Port Variables

dsPort :: Int
dsPort = 8080

asPort :: Int
asPort = 8090

lsPort :: Int
lsPort = 8091

tsPort :: Int
tsPort = 8092

-- | Host Variables

host :: String
host = "localhost"

dsHost :: String
dsHost = "localhost"

fsHost :: String
fsHost = "localhost"

lsHost :: String
lsHost = "localhost"

-- | Data Declarations

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

data FileMapping = FileMapping { fileName :: String
                               , serverName :: String
                               , serverPort :: String
                               } deriving (Show, Generic, FromJSON, ToJSON, FromBSON, ToBSON)

data TransactionData = TransactionData { userid :: String
                                       , servFileName :: String
                                       , tempFileName :: String
                                       , tServerPort :: String
                                       } deriving (Show, Generic, FromJSON, ToJSON, FromBSON, ToBSON)

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

data Lock = Lock { lockFileName :: String
                 , lockStatus :: Bool
                 , lockTime :: String
                 , lockID :: String -- Unique ID identifying the user who created the lock
                 } deriving (Show, Generic, FromJSON, ToJSON, FromBSON, ToBSON)

deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String

deriving instance FromBSON Bool  -- we need these as BSON does not provide
deriving instance ToBSON   Bool


-- | API Declarations

type FileServerAPI = "upload"     :> ReqBody '[JSON] SecureFileUpload :> Post '[JSON] SecureResponseData
                :<|> "delete"     :> ReqBody '[JSON] SecureFileName   :> Post '[JSON] SecureResponseData
                :<|> "download"   :> Get '[JSON] [String] --Doesn't need to be encrypted as only called from Directory Server
                :<|> "download"   :> ReqBody '[JSON] SecureFileName   :> Post '[JSON] SecureFile
                :<|> "modifyTime" :> ReqBody '[JSON] SecureFileName   :> Post '[JSON] SecureTime
                :<|> "commit"     :> Capture "sys" String             :> Capture "temp" String :> Get '[JSON] ResponseData

type DirectoryServerAPI = "search"      :> ReqBody '[JSON] SecureFileName   :> Post '[JSON] SecurePort
                     :<|> "searchMany"  :> Capture "fileName" String        :> Get '[JSON] [Int]
                     :<|> "upload"      :> ReqBody '[JSON] SecureFileUpload :> Post '[JSON] SecureResponseData
                     :<|> "list"        :> ReqBody '[JSON] SecureTicket     :> Post '[JSON] [String]
                     :<|> "updateList"  :> Capture "updateType" String      :> Capture "port" Int :> Capture "name" String :> Get '[JSON] ResponseData

type AuthenticationServerAPI = "login"      :> ReqBody '[JSON] LoginRequest :> Post '[JSON] AuthToken
                          :<|> "addNewUser" :> Capture "username" String    :> Capture "password" String :> Get '[JSON] ResponseData

type LockServerAPI = "lock"      :> ReqBody '[JSON] SecureFileName :> Post '[JSON] SecureResponseData
                :<|> "unlock"    :> ReqBody '[JSON] SecureFileName :> Post '[JSON] SecureResponseData
                :<|> "checkLock" :> ReqBody '[JSON] SecureFileName :> Post '[JSON] Bool

type TransactionServerAPI = "start"   :> ReqBody '[JSON] SecureTicket     :> Post '[JSON] SecureResponseData
                       :<|> "search"  :> ReqBody '[JSON] SecureFileName   :> Post '[JSON] SecureFile
                       :<|> "cache"   :> ReqBody '[JSON] SecureFileName   :> Post '[JSON] SecureResponseData
                       :<|> "upload"  :> ReqBody '[JSON] SecureFileUpload :> Post '[JSON] SecureResponseData
                       :<|> "commit"  :> ReqBody '[JSON] SecureTicket     :> Post '[JSON] SecureResponseData
                       :<|> "abort"   :> ReqBody '[JSON] SecureTicket     :> Post '[JSON] SecureResponseData

-- | Encryption Variable and Functions

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
