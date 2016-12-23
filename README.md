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

3. Caching (Client side to start)

- ~~Create a temp folder when the client is started~~
- ~~Check the temp folder first whenever a download is called~~
- ~~Update the temp folder whenever a new download occurs~~
- ~~Clear the temp folder when the client is closed~~
- ~~Delete the oldest file when the cache is full and a new file is downloaded~~
- ~~Figure out invalidation of files~~
- Make the cache module less hardcoded (localhost and port number)
  * Might make more sense once directory server has been created

4. Directory Server

- Switch to using mongoDB
- Save FileMapping to DB
- Figure out update logic
- 

5. Replication

6. Security Service

7. Transactions

8. Lock Service

## Comments:

Need to figure out how to have a single APIs.hs file that all of the different modules use.

The caching currently uses polling every minute <- Something which should be discussed in the report.

The fileserver and caching both works with local directories (files/ and temp/ respectively).

Caching should ideally be slightly more modular with respect to the polling.

