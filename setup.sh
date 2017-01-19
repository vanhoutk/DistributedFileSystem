sudo docker run -i -t ubuntu /bin/bash
mkdir -p /home/Documents/CS4532
cd /home/Documents/CS4532
apt-get update
apt-get install git curl libgtk2.0


git clone  https://github.com/vanhoutk/DistributedFileSystem.git

apt-key adv --keyserver keyserver.ubuntu.com --recv 7F0CEB10
echo 'deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen' | tee /etc/apt/sources.list.d/10gen.list
apt-get update && apt-get install mongodb-10gen


curl -sSL https://get.haskellstack.org/ | sh

cd DistributedFileSystem/client
stack setup