#sudo apt-get install git;
#git clone https://github.com/bgwines/hueue.git;
#cd hueue;
sudo apt-get install emacs24;
curl -sSL https://get.haskellstack.org/ | sh;
stack setup;
sudo apt-get install libsnappy-dev;
wget https://leveldb.googlecode.com/files/leveldb-1.9.0.tar.gz;
tar -xzf leveldb-1.9.0.tar.gz;
cd leveldb-1.9.0; make;
sudo mv libleveldb.* /usr/local/lib;
cd include; sudo cp -R leveldb /usr/local/include; sudo ldconfig;
cd ../..; stack build;
sudo apt-get install sqlite
