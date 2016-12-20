{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module APIs where

import           Data.Aeson
import           Data.Aeson.TH
import           GHC.Generics
import           Servant

data File = File  { name :: String
                  , contents :: String
                  } deriving (Show, Generic, FromJSON, ToJSON)


data ResponseData = ResponseData { response :: String
                                 } deriving (Generic, ToJSON, FromJSON, Show)

-- Could allow directory change
-- Could allow files to be moved
-- Could allow files to be deleted
-- Could allow directories to be created
type FileServerAPI = "upload"   :> ReqBody '[JSON] File  :> Post '[JSON] ResponseData
                :<|> "download" :> Get '[JSON] [String]
                :<|> "download" :> Capture "name" String :> Get '[JSON] File

type DirectoryServerAPI = "search" :> Capture "name" String :> Get '[JSON] Int