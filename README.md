# Distributed File System

Name: Kris Vanhoutte

Student Number: 12301975

## TODO:

1. Fill in README for:
  - Transaction Server
  - Client 
  - Directory Server
  - File Server

2. Write a "setup.sh" which downloads the necessary libraries

3. Informal Report
  
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