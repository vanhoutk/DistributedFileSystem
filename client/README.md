# Distributed File System

Name: Kris Vanhoutte

Student Number: 12301975

## Client Application

When the client first starts up, the user is requested to log in. A request is sent to the authentication service for an authentication token, and this token is passed to both the cache and the editor. The cache is described in section 9.

The Editor is the main interface which the client interacts with, although some console interaction is required as well. Through the Editor the client can choose to create a new file, read or read/write a file, see a list of all the existing files, upload a file or start a transaction. File names need to be input through the console when reading files from the file server, and the file list/log messages are displayed in the console.

The Editor disables the use of certain buttons when a transaction is not occurring and when a transaction is occurring. This prevents the client from circumventing the transaction. It was decided that for the proof of concept this was a suitable mechanism, although other options could be explored.

Given more time, an editor which does not require any console interaction would be created.

A client API handles all of the backend of the client, such that most of the operation of the client application is hidden from view, although it is visible through the console logging which is occurring. The client API handles the requests to all of the other services in the distributed file system, as well as interacting with the cache in the case of file interactions.

## Client Cache

Caching of files was implemented on the client side, by storing a copy of the file on disk on the client. As an upload/download model was used, it was decided that storing a copy in memory on the server side wasn’t useful, and that storing on disk on the client provided a satisfactory result. The cache is cleared when the client application is closed.

The cache service is closely linked to the client API which is handles the backend of the client application. The cache service checks for invalidation of files through polling every five minutes, although this time period could easily be extended or shortened. The cache has a limited size, and an LRU policy is used to remove files when the cache exceeds the allotted size.
The cache is passed the authentication token which the user receives on login so that its network accesses can all be encrypted.

The logic behind cache invalidation is shown below.

1. Check for Invalidation
	- Logic:
  	- Wait 5 minutes
  	- List all of the files currently in the cache (“/temp” folder)
  	- For each file in the cache:
    	- Get the local access time of the file
    	- Request the port of the fileserver from the directory server
    	- Get the modification time of the file on the file server
    	- Check if the modification time on the file server is later than the access time of the local file
      	- If so, download the remote file and updated the local copy
      	- Otherwise, do nothing