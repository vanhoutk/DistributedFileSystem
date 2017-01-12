# Distributed File System

Name: Kris Vanhoutte

Student Number: 12301975

## TODO:

1. Secondary Client Application

  - ~~Interactive~~
  - ~~Includes the first Client Application~~
  - Change into more of a text editor
  - Change upload logic to upload a file rather than text input

2. Primary Client Application

  - ~~Controls all of the API Calls~~
  - ~~Switch to using the directory server to look for files~~
  - Figure out how to decide which fileserver to upload to

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
  - Add upload logic for client/server interaction
  - ~~Move the Directory Server port to the common APIs folder~~

5. FileServer
  - ~~Add delete functionality~~
  - ~~On update/delete send an update to the directory server~~

6. Replication
  - TODO: Figure out how this should work
  - We examine replication in detail in lectures, and when we do so, you should think about the type of file usage that occurs in a typical file system. For example, multiple users do not simultaneously modify most files. So very tight synchronisation is probably not necessary – locking or transactions will probably be acceptable for those cases that do. Your task is to implement an appropriate replicatino model for file access across selected file servers (you should demonstrate operation with three or more).

  - A scheme based on the gossip architecture is possible, but a model based on a primary copy will most likely be good enough. If you choose to implement a gossip style solution, and an NFS style file system model is being implemented, then replicas should talk frequently to each other. If an upload/download model is in place then communication need only occur when files are closed!

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
  - TODO: Figure out how this should work
  - There are two levels to providing a transaction service in a distributed file system. First, it is necessary to extend your file server to support transactional modification of files on a single file server. This is probably best achieved in this project via a technique known as shadowing. Second, the concept of a transaction involving many files, possibly on many different file servers must be supported. This is probably best achieved via a separate and distinct transaction service. This service would employ a logging approach to record the steps of a distributed transaction, keeping track of which file servers and files are part of the transaction.

  - A reasonably straightforward algorithm is sufficient to implement distributed transactions for the purposes of this project. The algorithm works as follows:

  - If a set of transactional changes are to be performed, the client proxy signals this by requesting that the transaction server begin a transaction. It must then delegate all file modifications to this server rather than seek to perform the changes by communicating directly with the relevant file servers. It must signal the end of the transaction when all required changes have been communicated.

  - The transaction server begins a transaction by asking each server that becomes part of the transaction (by virtue of being asked to perform file modifications) to enter a “ready to commit” state. Each server performs necessary file updates transactionally (i.e. most likely to the shadow copy of the file). When ready to commit, they inform the transaction server. If all servers respond positively that they are ready-to-commit when asked, then the first phase is over. The transaction server records this fact in the log. At this point, even if the transaction server fails, the transaction can be completed when it restarts. If one or more servers fail to respond positively, then the transaction is aborted. The transaction server requests each constituent server to commit (or abort). When all respond, the transaction is complete.

  - If servers fail during step 3, so that the transaction is not completed at all nodes, the transaction server must continue to try to get the failed nodes to commit. Each of those nodes have already indicated (when responding positively at step 2) that they could commit. This implied that they could commit the transaction if subsequently asked to, even if they crash or become disconnected/partitioned during that process. Therefore, when contact is re-established with the failed server, the commit process can be restarted. If the server crashed, then the transaction will be recorded and thus recoverable in a ready to commit state. Note that if a server never restarts or its data is irrecoverably trashed, then the transaction cannot be completed - you do not need to worry about this pathalogical state for the purposes of this project work.

  - Your system should employ a variation on this basic algorithm. In general terms your system might work as follows. The client proxy will provide a simple API as an adjunct to (or perhapos integrated with) the basic file API to allow clients create and commit or abort transactions. When a transaction is begun, a distributed transaction server is contacted and a transaction identifier is generated. All file modifications subsequently made that are associated with this identifier are performed as part of a distributed transaction. When a file is opened, the transaction server is informed and it records which server and file is being opened. That server is now part of the transaction and will be asked later to commit the changes by the transaction server. Each transactional modification will be performed at the appropriate file server via shadowing. Eventually, the client will either abort or commit the transaction. In each case, the transaction server is informed and completes the processing by implementing the algorithm above in partnership with the file servers.

  - This general design has its faults – improve on it if you can.



9. Lock Service
  - ~~Create a locking server~~
  - ~~Add locking server functions to client API~~
  - ~~Allow client to make use of locking~~
  - ~~Added a ten minute timeout to locks~~
  - Could add current user who locked the file potentially (discuss in report)
  - ~~Clear locks on upload~~
    * Note: No check that the client who unlocks had the lock. Assuming only uploading files that had write access.

10. Add Proper Logging Messages
  - ~~LockServer~~
  - Client
  - Cache
  - ~~AuthenticationServer~~
  - DirectoryServer
  - FileServer
  - TransactionServer
  - ReplicationServer

11. Comment Code
  - ~~LockServer~~
  - ~~CommonAPIs~~
  - Client
  - Cache
  - ~~AuthenticationServer~~
  - DirectoryServer
  - FileServer
  - TransactionServer
  - ReplicationServer
  
## Comments:

The caching currently uses polling every minute <- Something which should be discussed in the report.

The fileserver and caching both works with local directories (files/ and temp/ respectively).

Caching should ideally be slightly more modular with respect to the polling.

No check on the fact that the sessionKey (or rather the ticket hasn't been tampered with) <- Discuss in report
Didn't put a time check on the ticket query for the file modify time. Too much effort and not very important.

TimeOut might not be fully secure, should possibly be encrypted with the sessionKey first, then the sharedSecret. Otherwise a new connection could be used to impersonate a different user. Potentially.