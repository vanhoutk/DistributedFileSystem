# Distributed File System

Name: Kris Vanhoutte

Student Number: 12301975

## Transaction Server

The transaction service allows the user to make group changes to files, by storing temporary copies of the files on fileservers, and only committing the changes once the client has finished and decided to commit them, or deleting those changes if the client decides to abort the transaction.

A basic transaction service was implemented for this project. When the client decides to start a transaction, all subsequent actions which the client performs are passed through the transaction service, until the client decides to commit or abort the transaction.

The transaction service uses two MongoDB databases, called TRANSACTIONS and TRANSACTION_MAPPINGS respectively. The first of these solely stores the unique transaction ID, which is the Session Key of the user who is asking for the transaction. The second database stores the information about each transaction, the transaction ID, the original filename, the temporary filename and the fileserver port.

The Session Key was used as the transaction ID because it was already present and should be unique. A new random transaction ID could be generated instead if desired, but was not required for this project.

It should be noted that functionality was not implemented to allow a client to upload new files through a transaction. Rather, transactions only allow the editing of currently existing files. Functionality for the uploading of new files could easily be added, but it was felt that this wasn’t required for this project.

The transaction service has a number of different functions, each of which is listed below.

1. Start Transaction
  - Inputs: 
    - Session Key encrypted with the Shared Server Secret
    - Timeout on the Authentication Token, encrypted with the Shared Server Secret
  - Return Values:
    - Response encrypted with the Session Key
  - Logic:
    - Decrypt the timeout
    - Decrypt the session key
    - Check if the token is still valid
      - If not, return a failed response
    - Store the transaction ID (Session Key) in the transaction database
    - Return a success response

2. Implement a download transaction
  - Inputs: 
    - Session Key encrypted with the Shared Server Secret
    - Timeout on the Authentication Token, encrypted with the Shared Server Secret
    - File name encrypted with the Session Key
  - Return Values:
    - File encrypted with the Session Key
  - Logic:
    - Decrypt the timeout
    - Decrypt the session key
    - Decrypt the file name
    - Check if the token is still valid
      - If not
        - Delete the transaction in the transaction database
        - Abort the transaction (see details later)
        - Return a failed response
    - Request a list of all fileserver ports for this file from the directory server
      - If this fails, return an error file
    - Add new mappings for each of the ports, with the original file name and temporary file name both the same
    - Download the file from the first port
      - If the download fails, return an error file
      - Otherwise return the file.

3. Update maps with a cached transaction
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
      - If not
        - Delete the transaction in the transaction database
        - Abort the transaction (see details later)
        - Return a failed response
    - Search the transaction mapping database to see if a mapping already exists
      - If so, return a success message
      - Otherwise
        - Request a list of all fileserver ports for this file from the directory server
          - If this fails, return a failed response
          - Otherwise
            - Add new mappings for each of the ports
            - Return a success response

4. Implement an upload transaction
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
      - If not
        - Delete the transaction in the transaction database
        - Abort the transaction (see details later)
        - Return a failed response
    - Search the transaction mapping database to see if a mapping already exists
      - If not, return a failed message
      - Otherwise
        - Create a temporary file name by adding 	~ to the end of the file name
        - Upload the temporary file to each fileserver that has the original file
        - Update the transaction maps with the name of the temporary file
        - Return a success response

5. Commit Transaction
  - Inputs: 
    - Session Key encrypted with the Shared Server Secret
    - Timeout on the Authentication Token, encrypted with the Shared Server Secret
  - Return Values:
    - Response encrypted with the Session Key
  - Logic:
    - Decrypt the timeout
    - Decrypt the session key
    - Check if the token is still valid
      - If not
        - Delete the transaction in the transaction database
        - Abort the transaction (see details later)
        - Return a failed response
    - For each transaction mapping
      - If the original file name and temporary file name are different, send a commit message to the file server
      - Unlock the lock for that file
      - Delete the Transaction Mapping
    - Delete the transaction in the transaction database
    - Return a success response

6. Abort Transaction
  - Inputs: 
    - Session Key encrypted with the Shared Server Secret
    - Timeout on the Authentication Token, encrypted with the Shared Server Secret
  - Return Values:
    - Response encrypted with the Session Key
  - Logic:
    - Decrypt the session key
    - Abort the transaction:
      - For each transaction mapping
        - If the original file name and the temporary file name are different, send a delete message to the file server with the name of the temporary file.
        - Unlock the lock for that file
        - Delete the Transaction Mapping
    - Delete the transaction in the transaction database
    - Return a success response