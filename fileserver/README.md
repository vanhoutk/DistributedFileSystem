# Distributed File System

Name: Kris Vanhoutte

Student Number: 12301975

## File Server

This directory contains the file service for the distributed file system.

The file server stores files locally in a directory named with the port number the file server is running on. i.e. files8081
  - This allows for multiple file servers to be run on the same machine using different ports

There are five functions run by the file service:

1. Upload File

2. Delete File

3. Get File List

4. Download File

5. Get File Modify Time
  - Used when client caches are checking for invalidations of locally stored files

### Possible Future Additions

1. Could allow directory change

2. Could allow files to be moved

3. Could allow directories to be created