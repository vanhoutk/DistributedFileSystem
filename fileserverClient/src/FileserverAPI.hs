{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FileserverAPI where

import           Data.Aeson
import           Data.Aeson.TH
import           GHC.Generics
import           Servant

data File = File 	{ name :: String
                  , contents :: String
                  } deriving (Show, Generic, FromJSON, ToJSON)

instance ToJSON   File
instance FromJSON File

--data Message = Message { name    :: String
--                       , message :: String
--                       } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)


data ResponseData = ResponseData { response :: String
                                 } deriving (Generic, ToJSON, FromJSON, Show)

type API = "upload"   :> ReqBody '[JSON] File  :> Post '[JSON] Bool
      :<|> "download" :> Get '[JSON] [File]
      :<|> "download" :> Capture "name" String :> Get '[JSON] File