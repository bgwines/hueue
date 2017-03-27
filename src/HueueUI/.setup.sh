# New server setup
sudo apt-get -y update;
sudo apt-get -y install g++
sudo apt-get -y install emacs24;
sudo apt-get -y install sqlite;
sudo apt-get -y install libsnappy-dev;
sudo apt-get -y install libleveldb-dev;
sudo apt-get -y install zlib1g-dev;
curl -sSL https://get.haskellstack.org/ | sh;

git clone https://github.com/bgwines/hueue.git;
cd hueue;
stack setup; stack build; stack install;

