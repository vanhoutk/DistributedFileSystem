#!/bin/sh
loc="$PWD"
echo "Building Directory Server"
cd "$loc/directoryServer"
stack build
echo "Building File Server"
cd  "$loc/fileserver"
stack build
echo "Building Authentication Server"
cd  "$loc/authenticationServer"
stack build
echo "Building Lock Server"
cd  "$loc/lockServer"
stack build
echo "Building Client"
cd  "$loc/client"
stack build