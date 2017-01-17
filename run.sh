#!/bin/sh
#kill `cat ./my.term.pid`
loc="$PWD"
cd "$loc"

dbloc="$HOME/mongoFiles"
mkdir -p "$dbloc"

gnome-terminal -e 'bash -c "mongod --dbpath $HOME/mongoFiles; exec bash"' &
echo $! > ./my.term.pid
gnome-terminal -e 'bash -c "cd '$loc'/fileserver;echo $PPID;stack exec fileserver-exe 8082; exec bash"'  &
echo $! >> ./my.term.pid
gnome-terminal -e 'bash -c "cd '$loc'/fileserver;echo $PPID;stack exec fileserver-exe 8081; exec bash"' &
echo $! >> ./my.term.pid
gnome-terminal -e 'bash -c "cd '$loc'/authenticationServer;echo $PPID;stack exec authenticationServer-exe; exec bash"'  &
echo $! >> ./my.term.pid
gnome-terminal -e 'bash -c "cd '$loc'/lockServer;echo $PPID;stack exec lockServer-exe; exec bash"'  &
echo $! >> ./my.term.pid
gnome-terminal -e 'bash -c "cd '$loc'/transactionServer;echo $PPID;stack exec transactionServer-exe; exec bash"'  &
echo $! >> ./my.term.pid
sleep 2  # Allows time for the database & fileservers to get online
gnome-terminal -e 'bash -c "cd '$loc'/directoryServer;echo $PPID;stack exec directoryServer-exe; exec bash"'  &
echo $! >> ./my.term.pid
sleep 2
gnome-terminal -e 'bash -c "cd '$loc'/client;echo $PPID;stack exec client-exe; exec bash"'  &
echo $! >> ./my.term.pid

curl http://localhost:8090/addNewUser/username/password