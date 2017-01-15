#!/bin/sh
loc="$PWD"
echo "Cleaning Directory Server"
cd "$loc/directoryServer"
stack clean
echo "Cleaning File Server"
cd  "$loc/fileserver"
stack clean
echo "Cleaning Authentication Server"
cd  "$loc/authenticationServer"
stack clean
echo "Cleaning Lock Server"
cd  "$loc/lockServer"
stack clean
echo "Cleaning Client"
cd  "$loc/client"
stack clean
echo "Cleaning Transaction Server"
cd  "$loc/transactionServer"
stack clean