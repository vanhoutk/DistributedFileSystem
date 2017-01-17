# Distributed File System

Name: Kris Vanhoutte

Student Number: 12301975

## TODO:

1. Check random number generator in Directory Server

2. Fill in README for Transaction Server (and check others)

3. ~~Add extra check to the lock service:~~
  - ~~Use the session key as a unique id and compare against it when locking/unlocking~~

4. ~~Comments and Logging for:~~
  - ~~Client~~
  - ~~Transaction Server~~
  - ~~Replication (Directory Server)~~

5. Write a "setup.sh" which downloads the necessary libraries

6. ~~Change clean.sh, build.sh and run.sh to more generic forms (i.e. make the paths generic)~~

7. ~~Add mkdir mongoFiles to run.sh~~

8. ~~Add default user to Authentication Server in run.sh (localhost:8090/addNewUser/username/password)~~

9. Possibly (hopefully not) make docker compatible
  
## Comments:

The caching currently uses polling every minute <- Something which should be discussed in the report.

The fileserver and caching both works with local directories (files/ and temp/ respectively).

Caching should ideally be slightly more modular with respect to the polling.

No check on the fact that the sessionKey (or rather the ticket hasn't been tampered with) <- Discuss in report
Didn't put a time check on the ticket query for the file modify time. Too much effort and not very important.

TimeOut might not be fully secure, should possibly be encrypted with the sessionKey first, then the sharedSecret. Otherwise a new connection could be used to impersonate a different user. Potentially.

SessionKey used as unique ID for user in transaction server <- Comment on in report

Don't currently allow user to reconnect without closing and re-opening the client - Report

Transaction Server does not allow you to add new files to the server (must be done outside transaction)

Not currently a closeall windows on exit command