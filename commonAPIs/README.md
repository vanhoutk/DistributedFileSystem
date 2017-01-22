# Distributed File System

Name: Kris Vanhoutte

Student Number: 12301975

## Common APIs

This directory contains two files which are imported by all the other services.

1. APIs.hs
  - Logging Variables
  - Port Variables
  - Data Declarations
  - API Declarations
  - Security Variables and Functions

2. MongoFunctions.hs
  - Taken from the Stephen Barrett's use-haskell project located here: https://bitbucket.org/esjmb/use-haskell
  - Contains a number of helper functions for mongoDB use with Haskell

Log messages are implemented in each service, but can be turned off by changing the corresponding variables in the APIs file.