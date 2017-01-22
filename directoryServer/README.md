# Distributed File System

Name: Kris Vanhoutte

Student Number: 12301975

## Directory Server

The directory service uses a MongoDB database, called FILE_SERVER_MAPPINGS, to store the information about the file:server mappings. The information which is stored in the database is: the name of the file, the name of the server on which it is stored, and the port number on which the fileserver is running.

When the directory server starts up, it requests a file list from each of the fileservers that it is connected to. As such, the fileservers need to be run in advance of this directory server. (This is implemented in the run.sh provided). The list of files on each server is then stored in the database.

In the case where a new file is being uploaded, the fileserver to upload to is randomly selected. This was done for simplicity, although a more complicated model based on the number of files or the size of the files on each server could easily be implemented instead.

Replication is implemented through the upload function of the directory server. As the transaction server also needs to maintain consistency between files, the directory server has a function which returns all of the fileservers that a file is on, which the transaction server uses for its own replication.

The directory service has a number of different functions, each of which is listed below.

1. Get a fileserver for a file
  - Inputs: 
    - Session Key encrypted with the Shared Server Secret
    - Timeout on the Authentication Token, encrypted with the Shared Server Secret
    - File name encrypted with the Session Key
  - Return Values:
    - Fileserver Port number encrypted with the Session Key
  - Logic:
    - Decrypt the timeout
    - Decrypt the session key
    - Decrypt the file name
    - Check if the token is still valid
      - If not:
        - Encrypt the port number 0 (which is used to signify error)
        - Return the encrypted error port
    - Search for the file name in the file mapping database
      - If the file mapping exists
        - Get the first file mapping (if there’s more than one)
        - Get the port from the file mapping
        - Encrypt the port
        - Return the encrypted port
      - Otherwise:
        - Encrypt the port number 0 (which is used to signify error)
        - Return the encrypted error port 

2. Get all fileservers for a file
  Inputs: 
    File name
  Return Values:
    List of port numbers
  Logic:
    Search for the file name in the file mapping database
      If the file mapping does not exist, return an empty list
      Otherwise:
        Get the port number for each file mapping and store in a list
        Return the list of port numbers 

3. Upload a file
  Inputs: 
    Session Key encrypted with the Shared Server Secret
    Timeout on the Authentication Token, encrypted with the Shared Server Secret
    File name encrypted with the Session Key
  Return Values:
    Response message encrypted with the Session Key
  Logic:
    Decrypt the timeout
    Decrypt the session key
    Decrypt the file name
    Check if the token is still valid
      If not, return a failed response
    Search for the file name in the file mapping database
      If a file mapping exists
        Upload the file to each of the fileservers 
        Note: There was no success check on upload here although this could easily be implemented by storing the return values of the mapM function.
        Return a success response
      Otherwise:
        Randomly select a fileserver
        Upload the file to this fileserver
          If this upload succeeds, return a success response
          Otherwise return a failed response.

4. List the files on all servers
  Inputs: 
    Session Key encrypted with the Shared Server Secret
    Timeout on the Authentication Token, encrypted with the Shared Server Secret
  Return Values:
    List of file names encrypted with the Session Key
  Logic:
    Decrypt the timeout
    Decrypt the session key
    Check if the token is still valid
      If not, return an encrypted list containing error information
    Get all of the file mapping which are currently stored
    Make a list of the file names in each file mapping
    Sort the list and remove any duplicate file names
    Encrypt the list
    Return the encrypted list

5. Update lists
  Inputs: 
    Type of update
    Port of the server sending the request
    Filename 
  Return Values:
    Response Message
  Logic:
    If the type is a “delete” update
      Calculate the server name from the port number
      Delete the file mapping for this filename and fileserver
      Return a success response
    If the type is an “update” update
      Calculate the server name from the port number
      Update the file mapping for this filename and fileserver
      Return a success response
    If the type is something else
      Return a failed response