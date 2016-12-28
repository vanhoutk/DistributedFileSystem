#!/bin/sh
#kill `cat ./my.term.pid`
loc="$PWD"
echo "Building directoryServer"
cd "$loc/directoryServer"
stack build
echo "Building fileServer"
cd  "$loc/fileserver"
stack build
echo "Building fileseverClient"
cd  "$loc/fileserver"
stack build
cd "$loc"
gnome-terminal -e 'bash -c "mongod --dbpath $HOME/mongoFiles; exec bash"' &
echo $! > ./my.term.pid
gnome-terminal -e 'bash -c "cd ~/Documents/CS4532/DFS/fileserver;echo $!;stack exec fileserver-exe 8081; exec bash"' &
echo $! 
echo $! >> ./my.term.pid
gnome-terminal -e 'bash -c "cd ~/Documents/CS4532/DFS/fileserver;echo $PPID;stack exec fileserver-exe 8082; exec bash"'  &
echo $! >> ./my.term.pid
gnome-terminal -e 'bash -c "cd ~/Documents/CS4532/DFS/directoryServer;echo $PPID;stack exec directoryServer-exe; exec bash"'  &
echo $! >> ./my.term.pid