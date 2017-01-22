# Distributed File System

Name: Kris Vanhoutte

Student Number: 12301975

## Lock Server

The locking service allows for the exclusive access of files in the distributed file system. Any time a client requests a file with read/write permissions, either through singly or as part of a transaction, a lock is requested for the file, and the file is accessed if and only if the request for a lock is successful.

The locking service uses a MongoDB database, called FILE_LOCKS, to store the information about the locks. The information which is stored in the database is: the name of the file, the status of the lock, the time the lock is valid until, and the session key of the user who locked it. A timeout of ten minutes is used for each lock; this prevents locks from being permanently locked if a client goes offline without unlocking the file. 

The session key of the user was used as a unique identifier for the user. While generating a unique identifier for the user would potentially be a better approach, it was decided that the session key was suitable for the purposes of this project. The unique identifier prevents other users from unlocking a file which they didn’t lock originally.

It was decided that locks would only be provided on files, and not on entire directories. There were a couple of reasons for this. The first was that as a flat file system was used, and only a small number of file servers were created, locking an entire file system could cause trouble for other users. 

The second reason was that it simplified logic and reduced network accesses. In the case where a lock already exists, to lock a directory, it would require a request to the directory server to find out the file server name for a particular file, followed by a lock request for that server. Whereas a single request is required when locking a file.

The program is designed in such a way that adding functionality to lock an entire directory would be relatively trivial if it was required.

The locking service has three functions within it, the logic of each is explained below.

1. Locking a file
  - Inputs: 
    - Session Key encrypted with the Shared Server Secret
    - Timeout on the Authentication Token, encrypted with the Shared Server Secret
    - File name encrypted with the Session Key
  - Return Values:
    - Response encrypted with the Session Key
  - Logic:
    - Decrypt the timeout
    - Decrypt the session key
    - Decrypt the file name
    - Check if the token is still valid
      - If not, return a failed response
    - Search for the file name in the lock database
      - If the lock exists and is currently locked
        - Check if 10 minutes has passed since it was locked
          - If so, return a success response and the update the lock with this user’s session key
          - Otherwise, return a failed response
      - Otherwise:
        - If a lock record exists but is unlocked
          - Change the status to locked, set a time out of ten minutes and return a success response
        - If no lock record exists
          - Creates a new one with a timeout of ten minutes and return a success response

2. Unlock a file
  - Inputs: 
    - Session Key encrypted with the Shared Server Secret
    - Timeout on the Authentication Token, encrypted with the Shared Server Secret
    - File name encrypted with the Session Key
  - Return Values:
    - Response encrypted with the Session Key
  - Logic:
    - Decrypt the timeout
    - Decrypt the session key
    - Decrypt the file name
    - Check if the token is still valid
      - If not, return a failed response
    - Search for the file name in the lock database
      - If the lock exists and is currently locked
        - If the user who sent this request is the one who locked the file
          - Unlock the file and send a success response
        - Otherwise:
          - Check if 10 minutes has passed since it was locked
            - If so, unlock the file and send a success response
            - Otherwise, return a failed response
      - Otherwise:
        - Return a failed response

3. Check the lock on a file
  - Inputs: 
    - Session Key encrypted with the Shared Server Secret
    - Timeout on the Authentication Token, encrypted with the Shared Server Secret
    - File name encrypted with the Session Key
  - Return Values:
    - True/False whether or not the file is locked
  - Logic:
    - Decrypt the timeout
    - Decrypt the session key
    - Decrypt the file name
    - Check if the token is still valid
      - If not, return True (stops the client from getting a lock)
    - Search for the file name in the lock database
      - If the lock exists and is currently locked
        - Check if 10 minutes has passed since it was locked 
          - If so, unlock the file and return False
          - Otherwise, return True
      - Otherwise:
        - Return False