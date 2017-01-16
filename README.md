# Distributed File System

Name: Kris Vanhoutte

Student Number: 12301975

## TODO:

1. Secondary Client Application

  - ~~Interactive~~
  - ~~Includes the first Client Application~~
  - ~~Change into more of a text editor~~
  - ~~Change upload logic to upload a file rather than text input~~

2. Primary Client Application

  - ~~Controls all of the API Calls~~
  - ~~Switch to using the directory server to look for files~~
  - ~~Figure out how to decide which fileserver to upload to~~

3. Caching (Client side to start)

  - ~~Create a temp folder when the client is started~~
  - ~~Check the temp folder first whenever a download is called~~
  - ~~Update the temp folder whenever a new download occurs~~
  - ~~Clear the temp folder when the client is closed~~
  - ~~Delete the oldest file when the cache is full and a new file is downloaded~~
  - ~~Figure out invalidation of files~~
  - ~~Make the cache module less hardcoded (localhost and port number)~~

4. Directory Server

  - ~~Switch to using mongoDB~~
  - ~~Save FileMapping to DB~~
  - ~~Figure out update logic~~
  - ~~Add upload logic for client/server interaction~~
  - ~~Move the Directory Server port to the common APIs folder~~
  - **Check that random number gen in upload logic works correctly**
  - Remove duplicates when listing files

5. FileServer
  - ~~Add delete functionality~~
  - ~~On update/delete send an update to the directory server~~

6. Replication
  - ~~Implemented through a centralised replication server (The Directory Server)~~

7. ~~Security Service~~

  - ~~Authentication Server~~
  - ~~Add a timeout to the token~~
  - ~~Add authentiction to:~~
    * ~~Client~~
      - ~~Switch order of cache and login in start of client and pass token to cache~~
    * ~~Cache~~
    * ~~APIs~~
      - ~~Figure out how to encrypt the [String], UTCTime and Int responses.~~
  - **Should possibly encrypt the timeout with the user's password?**

8. Transactions
  - ~~Will use the user's session key as a unique identifier for the user~~
  - ~~Need to add an abort/commit to the fileserver~~
  - Fill in the README
  

9. Lock Service
  - ~~Create a locking server~~
  - ~~Add locking server functions to client API~~
  - ~~Allow client to make use of locking~~
  - ~~Added a ten minute timeout to locks~~
  - ~~Clear locks on upload~~
    * Note: No check that the client who unlocks had the lock. Assuming only uploading files that had write access.
  - Could add current user who locked the file potentially (discuss in report) - Could use the user's session key as in transaction server...

10. Add Proper Logging Messages
  - ~~LockServer~~
  - ~~AuthenticationServer~~
  - ~~DirectoryServer~~
  - ~~FileServer~~
  - Client
  - Cache
  - TransactionServer
  - ReplicationServer

11. Comment Code
  - ~~LockServer~~
  - ~~CommonAPIs~~
  - ~~Cache~~
  - ~~AuthenticationServer~~
  - ~~DirectoryServer~~
  - ~~FileServer~~
  - ClientAPI
  - TransactionServer
  - ReplicationServer
  
## Comments:

The caching currently uses polling every minute <- Something which should be discussed in the report.

The fileserver and caching both works with local directories (files/ and temp/ respectively).

Caching should ideally be slightly more modular with respect to the polling.

No check on the fact that the sessionKey (or rather the ticket hasn't been tampered with) <- Discuss in report
Didn't put a time check on the ticket query for the file modify time. Too much effort and not very important.

TimeOut might not be fully secure, should possibly be encrypted with the sessionKey first, then the sharedSecret. Otherwise a new connection could be used to impersonate a different user. Potentially.

SessionKey used as unique ID for user in transaction server <- Comment on in report