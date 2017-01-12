# Distributed File System

Name: Kris Vanhoutte

Student Number: 12301975

## Authentication Server

This directory contains the authentication service for the distributed file system.

The two functions of the authentication service are:

1. Login User
  - Search for a user in the database
  - If a user is not found return an error message
  - Otherwise:
    - Decrypt the message the user sent with the stored password
    - If the message does not match the username, return an error.
    - Otherwise:
      - Generate a random string for the session key
      - Encrypt the session key with the user's password to create an encrypted session key
      - Encrypt the session key with the shared server secret to create an authentication ticket
      - Encrypt the ticket with the user's password to create an encrypted ticket
      - Generate a timeout for the token (the token contains the ticket, session key and timeout), of current time + 1 Hour
      - Encrypt the timeout with the shared server secret so that users can't change the timeout.
      - Send the token back to the user

2. Add New User
  - For use by "Admins"
  - Adds a new username and password tuple to the database
  - Stored in the database unencrypted, but could easily have encryption added.