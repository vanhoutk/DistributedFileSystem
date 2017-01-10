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

module AS
    ( startAuthentication
    ) where

import            Control.Monad
import            Control.Monad.IO.Class
import            Control.Monad.Trans.Except
import            Control.Monad.Trans.Resource
import            Data.Aeson
import            Data.Bson.Generic
import qualified  Data.List                     as DL
import            Data.Maybe
import            Data.Proxy
import            Data.Text                     (pack, unpack)
import            Data.Time
import            Database.MongoDB
import            Network.HTTP.Client           (newManager, defaultManagerSettings)
import            Network.Wai
import            Network.Wai.Handler.Warp
import            Servant
import qualified  Servant.API                   as SC
import qualified  Servant.Client                as SC
import            System.Directory
import            System.Random
import            APIs
import            MongoFunctions

type APIHandler = ExceptT ServantErr IO

startAuthentication :: IO()
startAuthentication = do
  putStrLn "Starting Authentication Server..."
  str <- generateRandomString
  putStrLn $ "Randomly generated string: " ++ str
  let encStr = encryptDecrypt "key" str
  let decStr = encryptDecrypt "key" encStr
  putStrLn $ "Encrypted string: " ++ encStr
  putStrLn $ "Decrypted string: " ++ decStr
  run asPort app

generateRandomString :: IO String
generateRandomString = liftM (take 10 . randomRs ('a','z')) newStdGen

app :: Application
app = serve api server

api :: Proxy AuthenticationServerAPI
api = Proxy

server :: Server AuthenticationServerAPI
server = loginUser
    :<|> addNewUser

  where

    loginUser :: LoginRequest -> APIHandler AuthToken
    loginUser (LoginRequest userName encMessage) = do
      userAccount <- liftIO $ withMongoDbConnection $ do
        userAccount' <- find (select ["username" =: userName] "USER_ACCOUNTS") >>= drainCursor
        return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe AccountData) userAccount'

      liftIO $ print userAccount
      case (length userAccount) of
        0 -> return (AuthToken "Failed" "Username not found" "Failed")
        _ -> do
          let (AccountData _ password) = head userAccount
          let decMessage = encryptDecrypt password encMessage
          if (decMessage == userName) then do
            sessionKey <- liftIO $ generateRandomString
            let encSessionKey = encryptDecrypt password sessionKey
            let decSessionKey = encryptDecrypt password encSessionKey
            let ticket = encryptDecrypt sharedServerSecret sessionKey
            let encTicket = encryptDecrypt password ticket

            currentTime <- liftIO $ getCurrentTime
            let oneHour = 60 * 60
            let tokenTimeOut = addUTCTime oneHour currentTime
            let encTimeOut = encryptTime sharedServerSecret tokenTimeOut 

            liftIO $ do
              putStrLn $ "SessionKey: " ++ sessionKey
              putStrLn $ "EncSessionKey: " ++ encSessionKey
              putStrLn $ "DecSessionKey: " ++ decSessionKey
              putStrLn $ "Ticket: " ++ ticket
              putStrLn $ "EncTicket: " ++ encTicket
            return (AuthToken encTicket encSessionKey encTimeOut)
          else do
            return (AuthToken "Failed" "Encryption failed" "Failed")

    addNewUser :: String -> String -> APIHandler ResponseData
    addNewUser username password = do
      liftIO $ do
        putStrLn $ "Inserting new user: " ++ username ++ " with password: " ++ password
        let accountData = (AccountData username password)
        withMongoDbConnection $ upsert (select ["username" =: username] "USER_ACCOUNTS") $ toBSON accountData 
      return (ResponseData "Successfully added new user")
