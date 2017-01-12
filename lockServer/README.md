# Distributed File System

Name: Kris Vanhoutte

Student Number: 12301975

## Lock Server

This directory contains the locking service for the distributed file system.

The three functions of the locking service are:

1. Lock a file
  - Decrypts the session key
  - Decrypts the token time
  - Checks the token time is still valid
    - Returns an error message if not
  - Searches for a lock record for the file
    - If currently locked:
      - Checks if it's been 10 minutes since it was locked (timeout for locks)
        - If so, returns a success message and the new user gets the lock.
        - Otherwise, returns an error message
    - Otherwise:
      - If a lock record exists but is unlocked, changes to locked, sets a time out of ten minutes and returns a success message
      - If no lock record, creates a new one that is locked with a timeout of ten minutes and returns a success message.

2. Unlock a file
  - Decrypts the session key
  - Decrypts the token time
  - Checks the token time is still valid
    - Returns an error message if not
  - Searches for a lock record for the file
    - If currently locked:
      - Unlocks the file and returns a success message
    - Otherwise:
      - Returns a fail message

3. Check the lock on a file
  - Decrypts the session key
  - Decrypts the token time
  - Checks the token time is still valid
    - Returns an error message if not
  - Searches for a lock record for the file
    - If currently locked:
      - Checks if it's been 10 minutes since it was locked (timeout for locks)
        - If so, unlocks the file and returns False
        - Otherwise, returns True
    - Otherwise:
      - Returns False