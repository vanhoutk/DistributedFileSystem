# Distributed File System

Name: Kris Vanhoutte

Student Number: 12301975

## Authentication Server

The authentication service provides a level of security to the distributed file system. The authentication scheme used within this project is the three key security model which was provided in the project description. A shared server secret was used to simplify the scheme, although storing individual server keys could be done without too much difficulty. It would just increase the number of requests the authentication server receives from each client. 

Encryption and decryption are done through the use of a simple XOR function. This encryption mechanism was chosen because it provided simple and easy to implement symmetric key encryption. A more secure encryption scheme could be implemented, but was not deemed necessary for the scope of this project.

It should be noted that there are currently no checks in place in any of the services to see if the session key has been corrupted/tampered with. Logic for this could easily be added but it was decided that this was outside the scope for a proof of concept project.

The timeout for the authentication token was encrypted with the shared server secret to prevent the client from tampering with it and extending the validity of the token. This was not further encrypted with the user’s password, although adding the extra encryption would be trivial.

The authentication service uses a MongoDB database, called USER_ACCOUNTS, to store the username and password tuples. A function has been created for use by “admins” to add new users to this database. As only admins should be able to add new users, there is no requests to this endpoint through the client application. New users can be added to the database using the following command.

`curl http://localhost:8090/addNewUser/username/password`

The authentication service has two functions within it, the logic of each is explained below.

1. Login User
  - Inputs: 
    - Username
    - A message encrypted with the user’s password
  - Return Values:
    - Authentication token which contains:
    - A session key, encrypted twice, first with the Shared Server Secret, follow by the user’s password, which is the authentication ticket
    - A session key, encrypted with the user’s password
    - A timeout for the token, encrypted with the Shared Server Secret
  - Logic:
    - Search for the user in the user database
      - If the user does not exist
        - Return an error Authentication Token
      - Otherwise:
        - Decrypt the message the user sent with the stored password
        - If the message does not match the username
          - Return an error Authentication Token
        - Otherwise:
          - Generate a random string for the session key
          - Encrypt the session key with the user's password to create an encrypted session key
          - Encrypt the encrypted session key with the shared server secret to create an authentication ticket
          - Encrypt the ticket with the user's password to create an encrypted ticket
          - Generate a timeout for the token
          - Encrypt the timeout with the shared server secret so that users can't change the timeout.
          - Return the Authentication Token

2. Add New User
  - Inputs: 
    - Username
    - Password
  - Return Values:
    - Response message
  - Logic:
    - Add a new username and password tuple to the database if one doesn’t already exist for that username, or update the password if a tuple already exists.
    - Return a success response