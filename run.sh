#!/bin/sh
#kill `cat ./my.term.pid`
loc="$PWD"
cd "$loc"
gnome-terminal -e 'bash -c "mongod --dbpath $HOME/mongoFiles; exec bash"' &
echo $! > ./my.term.pid
gnome-terminal -e 'bash -c "cd ~/Documents/CS4532/DFS/fileserver;echo $PPID;stack exec fileserver-exe 8082; exec bash"'  &
echo $! >> ./my.term.pid
gnome-terminal -e 'bash -c "cd ~/Documents/CS4532/DFS/fileserver;echo $PPID;stack exec fileserver-exe 8081; exec bash"' &
echo $! >> ./my.term.pid
gnome-terminal -e 'bash -c "cd ~/Documents/CS4532/DFS/authenticationServer;echo $PPID;stack exec authenticationServer-exe; exec bash"'  &
echo $! >> ./my.term.pid
gnome-terminal -e 'bash -c "cd ~/Documents/CS4532/DFS/lockServer;echo $PPID;stack exec lockServer-exe; exec bash"'  &
echo $! >> ./my.term.pid
gnome-terminal -e 'bash -c "cd ~/Documents/CS4532/DFS/transactionServer;echo $PPID;stack exec transactionServer-exe; exec bash"'  &
echo $! >> ./my.term.pid
sleep 2
gnome-terminal -e 'bash -c "cd ~/Documents/CS4532/DFS/directoryServer;echo $PPID;stack exec directoryServer-exe; exec bash"'  &
echo $! >> ./my.term.pid
sleep 2
gnome-terminal -e 'bash -c "cd ~/Documents/CS4532/DFS/client;echo $PPID;stack exec client-exe; exec bash"'  &
echo $! >> ./my.term.pid