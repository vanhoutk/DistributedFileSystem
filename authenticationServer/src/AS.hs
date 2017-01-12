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
  logMessage authServerLogging ("Starting Authentication Server...")
  run asPort app

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
      liftIO $ logMessage authServerLogging ("Login Request for " ++ userName)
      userAccount <- liftIO $ withMongoDbConnection $ do -- Search for the user in the database
        userAccount' <- find (select ["username" =: userName] "USER_ACCOUNTS") >>= drainCursor
        return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe AccountData) userAccount'

      case (length userAccount) of
        0 -> do -- No user found
          liftIO $ logMessage authServerLogging ("Login Request failed. User " ++ userName ++ " not found.")
          return (AuthToken "Failed" "Username not found" "Failed")
        
        _ -> do -- User found
          let (AccountData _ password) = head userAccount
          liftIO $ logMessage authServerLogging ("User found:  " ++ userName ++ ". Password: " ++ password)
          let decMessage = encryptDecrypt password encMessage
          if (decMessage == userName) then do -- User used a correct password

            -- Create a token for the user
            sessionKey <- liftIO $ generateRandomString
            let encSessionKey = encryptDecrypt password sessionKey
            let ticket = encryptDecrypt sharedServerSecret sessionKey
            let encTicket = encryptDecrypt password ticket

            -- Generate a timeout for the token
            currentTime <- liftIO $ getCurrentTime
            let oneHour = 60 * 60
            let tokenTimeOut = addUTCTime oneHour currentTime
            let encTimeOut = encryptTime sharedServerSecret tokenTimeOut 

            liftIO $ logMessage authServerLogging ("SessionKey: " ++ sessionKey ++ "/n" ++
                                                    "EncSessionKey: " ++ encSessionKey ++ "/n" ++
                                                    "Ticket: " ++ ticket ++ "/n" ++
                                                    "EncTicket: " ++ encTicket ++ "/n" ++
                                                    "Timeout: " ++ (show tokenTimeOut) ++ "/n")

            return (AuthToken encTicket encSessionKey encTimeOut)
          else do -- User used an incorrect password
            liftIO $ logMessage authServerLogging ("Login Request failed. User " ++ userName ++ " used an incorrect password.")
            return (AuthToken "Failed" "Encryption failed" "Failed")

      where
        generateRandomString :: IO String
        generateRandomString = liftM (take 10 . randomRs ('a','z')) newStdGen

    addNewUser :: String -> String -> APIHandler ResponseData
    addNewUser username password = do
      liftIO $ do
        logMessage authServerLogging ("Inserting new user: " ++ username ++ " with password: " ++ password)
        let accountData = (AccountData username password)
        withMongoDbConnection $ upsert (select ["username" =: username] "USER_ACCOUNTS") $ toBSON accountData 
      return (ResponseData "Successfully added new user")
