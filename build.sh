#!/bin/sh
loc="$PWD"
echo "Building Directory Server"
cd "$loc/directoryServer"
stack setup
stack build
echo "Building File Server"
cd  "$loc/fileserver"
stack setup
stack build
echo "Building Authentication Server"
cd  "$loc/authenticationServer"
stack setup
stack build
echo "Building Lock Server"
cd  "$loc/lockServer"
stack setup
stack build
echo "Building Client"
cd  "$loc/client"
stack setup
stack build
echo "Building Transaction Server"
cd  "$loc/transactionServer"
stack setup
stack build