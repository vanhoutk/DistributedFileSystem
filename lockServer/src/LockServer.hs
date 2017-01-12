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

module LockServer
    ( startLockServer
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

startLockServer :: IO ()
startLockServer = do
  logMessage lockServerLogging ("Starting Lock Server...")
  run lsPort app

app :: Application
app = serve api server

api :: Proxy LockServerAPI
api = Proxy

server :: Server LockServerAPI
server = lockFile
    :<|> unlockFile
    :<|> checkLock

    where

      lockFile :: SecureFileName -> APIHandler SecureResponseData
      lockFile (SecureFileName ticket encTimeOut encName) = do
        let decTimeOut = decryptTime sharedServerSecret encTimeOut
        let sessionKey = encryptDecrypt sharedServerSecret ticket
        let decName = encryptDecrypt sessionKey encName

        liftIO $ logMessage lockServerLogging ("Locking file: " ++ decName)

        currentTime <- liftIO $ getCurrentTime
        
        if (currentTime > decTimeOut) then do
          liftIO $ logMessage lockServerLogging ("Client's authentication token has expired.")
          let encResponse = encryptDecrypt sessionKey "Failed - SessionKey has expired."
          return (SecureResponseData encResponse)
        
        else do -- Search for lock
          fileLock <- liftIO $ withMongoDbConnection $ do
            docs <- find (select ["lockFileName" =: decName] "FILE_LOCKS") >>= drainCursor
            return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
          
          case fileLock of
            [(Lock _ True time)] -> do -- Lock already exists.
              currentTime <- liftIO $ getCurrentTime
              let time' = read time :: UTCTime
              if(time' > currentTime) then do -- Lock still valid
                liftIO $ logMessage lockServerLogging ("File: " ++ decName ++ " already locked. Unable to create new lock.")
                let encResponse = encryptDecrypt sessionKey ("Lock on file " ++ decName ++ " failed - A Lock already exists for this file.")
                return (SecureResponseData encResponse)
              else do -- Lock has expired
                liftIO $ logMessage lockServerLogging ("File: " ++ decName ++ " lock has expired. Creating new lock...")
                
                currentTime <- liftIO $ getCurrentTime
                let tenMinutes = 10 * 60
                let lockTimeOut = addUTCTime tenMinutes currentTime
                
                liftIO $ do withMongoDbConnection $ upsert (select ["lockFileName" =: decName] "FILE_LOCKS") $ toBSON $ (Lock decName True (show(lockTimeOut) :: String))
                liftIO $ logMessage lockServerLogging ("File: " ++ decName ++ " locked successfully.")
                let encResponse = encryptDecrypt sessionKey ("Lock on file " ++ decName ++ " succeeded.")
                return (SecureResponseData encResponse) 
            
            _ -> do -- No lock exists.
              currentTime <- liftIO $ getCurrentTime
              let tenMinutes = 10 * 60
              let lockTimeOut = addUTCTime tenMinutes currentTime
              
              liftIO $ do withMongoDbConnection $ upsert (select ["lockFileName" =: decName] "FILE_LOCKS") $ toBSON $ (Lock decName True (show(lockTimeOut) :: String))
              liftIO $ logMessage lockServerLogging ("File: " ++ decName ++ " locked successfully.")
              let encResponse = encryptDecrypt sessionKey ("Lock on file " ++ decName ++ " succeeded.")
              return (SecureResponseData encResponse) 

      unlockFile :: SecureFileName -> APIHandler SecureResponseData
      unlockFile (SecureFileName ticket encTimeOut encName) = do
        let decTimeOut = decryptTime sharedServerSecret encTimeOut
        let sessionKey = encryptDecrypt sharedServerSecret ticket
        let decName = encryptDecrypt sessionKey encName

        liftIO $ logMessage lockServerLogging ("Unlocking file: " ++ decName)

        currentTime <- liftIO $ getCurrentTime

        if (currentTime > decTimeOut) then do
          liftIO $ logMessage lockServerLogging ("Client's authentication token has expired.")
          let encResponse = encryptDecrypt sessionKey "Failed - SessionKey has expired."
          return (SecureResponseData encResponse)
        
        else do -- Search for lock
          fileLock <- liftIO $ withMongoDbConnection $ do
            docs <- find (select ["lockFileName" =: decName] "FILE_LOCKS") >>= drainCursor
            return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
          
          case fileLock of
            [(Lock _ True time)] -> do -- Lock already exists.
              liftIO $ do withMongoDbConnection $ upsert (select ["lockFileName" =: decName] "FILE_LOCKS") $ toBSON $ (Lock decName False time)
              liftIO $ logMessage lockServerLogging ("File: " ++ decName ++ " unlocked successfully.")
              let encResponse = encryptDecrypt sessionKey ("Unlock of file " ++ decName ++ " succeeded.")
              return (SecureResponseData encResponse)
            
            _ -> do -- No lock exists.
              liftIO $ logMessage lockServerLogging ("File: " ++ decName ++ " is already unlocked.")
              let encResponse = encryptDecrypt sessionKey ("Unlock on file " ++ decName ++ " failed - File is already unlocked.")
              return (SecureResponseData encResponse)


      checkLock :: SecureFileName -> APIHandler Bool
      checkLock (SecureFileName ticket encTimeOut encName) = do
        let decTimeOut = decryptTime sharedServerSecret encTimeOut
        let sessionKey = encryptDecrypt sharedServerSecret ticket
        let decName = encryptDecrypt sessionKey encName --Decrypt the file name.

        liftIO $ logMessage lockServerLogging ("Checking lock on file: " ++ decName)

        currentTime <- liftIO $ getCurrentTime

        if (currentTime > decTimeOut) then do 
          liftIO $ logMessage lockServerLogging ("Client's authentication token has expired.")
          return True -- Token is invalid
        
        else do -- Search for lock
          fileLock <- liftIO $ withMongoDbConnection $ do 
            docs <- find (select ["lockFileName" =: decName] "FILE_LOCKS") >>= drainCursor
            return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
          
          case fileLock of
            [(Lock _ True time)] -> do -- File is locked
              currentTime <- liftIO $ getCurrentTime
              let time' = read time :: UTCTime -- Convert the string time to UTCTime
              if(time' > currentTime) then do -- Check if the lock is still valid
                liftIO $ logMessage lockServerLogging ("File: " ++ decName ++ " is locked.")
                return True
              else do
                liftIO $ logMessage lockServerLogging ("File: " ++ decName ++ " lock has expired, unlocking file...")
                liftIO $ do withMongoDbConnection $ upsert (select ["lockFileName" =: decName] "FILE_LOCKS") $ toBSON $ (Lock decName False time)
                return False
            
            _ -> do -- File is unlocked or no lock record exists
              liftIO $ logMessage lockServerLogging ("File: " ++ decName ++ " is unlocked.")
              return False