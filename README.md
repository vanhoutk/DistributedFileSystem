# Distributed File System

Name: Kris Vanhoutte

Student Number: 12301975

## TODO:

1. Secondary Client Application

  - ~~Interactive~~
  - ~~Includes the first Client Application~~
  - Change into more of a text editor

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
    * Might make more sense once directory server has been created
  - Possibly switch to using mongoDB for the cache and store file + port of server it's on for checking updates

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

7. Security Service

  - ~~Authentication Server~~
  - Add authentiction to:
    * ~~Client~~
      - ~~Switch order of cache and login in start of client and pass token to cache~~
    * Fileserver
      - Add logic for detection of incorrect encryption?
      - Can pass encTicket along with ticket and compare the decTicket with the ticket?
    * Directory Server
      - Add logic for detection of incorrect encryption?
    * ~~Cache~~
    * APIs
      - Figure out how to encrypt the [String], UTCTime and Int responses.

8. Transactions

9. Lock Service

## Comments:

The caching currently uses polling every minute <- Something which should be discussed in the report.

The fileserver and caching both works with local directories (files/ and temp/ respectively).

Caching should ideally be slightly more modular with respect to the polling.

