# Distributed File System

Name: Kris Vanhoutte

Student Number: 12301975

## File Server

The file service which was implemented was a flat file system, i.e. each file server provides a single directory. It was decided to implement a flat file system as it made file:server mappings within the directory service simpler for this proof of concept system. A layered directory structure could be implemented for the file service, although the directory service logic would need to be changed quite a bit.

The file service stores files locally to disk in a directory named with the port which the fileserver is running on, for example “files8081”. When a file is deleted or uploaded, the fileserver sends a message to the directory server updating its list of files for this fileserver.
It should be noted that there is no check for validity of authentication token when a modification time request is received. It was decided that for simplicity the modification time would be returned regardless, as adding functionality to the cache to deal with the error would have added an extra level of complexity.

When a transaction occurs, a temporary copy of a file is uploaded to the file server, this temporary copy is denoted with a ~ after its name. If the transaction is aborted these files are deleted. If the transaction is committed these files are copied into the local files and then deleted.

The file service has a number of different functions, each of which is listed below.

1. File Upload
	- Inputs: 
		- Session Key encrypted with the Shared Server Secret
		- Timeout on the Authentication Token, encrypted with the Shared Server Secret
		- File name encrypted with the Session Key
		- File contents encrypted with the Session Key
  - Return Values:
    - Response encrypted with the Session key
  - Logic:
    - Decrypt the timeout
    - Decrypt the session key
    - Decrypt the file name
    - Check if the token is still valid
      - If not, return a failed response
    - Decrypt the file contents
    - Send an update request to the directory server
    - Store the file
    - Return a success response

2. Delete a file
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
    - Send a delete request to the directory server
    - Remove the file
    - Return a success response

3. List the files
  - Return Values:
    - List of file names 
  - Logic:
    - List the contents of the directory
    - Sort the list
    - Return the list


4. File Download
  - Inputs: 
    - Session Key encrypted with the Shared Server Secret
    - Timeout on the Authentication Token, encrypted with the Shared Server Secret
    - File name encrypted with the Session Key
  - Return Values:
    - File name encrypted with the Session Key
    - File contents encrypted with the Session Key
  - Logic:
    - Decrypt the timeout
    - Decrypt the session key
    - Decrypt the file name
    - Check if the token is still valid
      - If not, return a failed response
    - Read the contents of the file
    - Encrypt the contents
    - Return the encrypted file name and contents 

5. Get Modify Time
  - Inputs: 
    - Session Key encrypted with the Shared Server Secret
    - Timeout on the Authentication Token, encrypted with the Shared Server Secret
    - File name encrypted with the Session Key
  - Return Values:
    - Modification time encrypted with the Session Key
  - Logic:
    - Decrypt the session key
    - Decrypt the file name
    - Get the modification time of the file
    - Encrypt the modification time
    - Return the encrypted modification time

6. Commit Files
  - Inputs: 
    - File name
    - Temporary file name
  - Return Values:
    - Response message
  - Logic:
    - Read the contents of the temporary file
    - Write these contents to the permanent file
    - Delete the temporary file
    - Return a response message