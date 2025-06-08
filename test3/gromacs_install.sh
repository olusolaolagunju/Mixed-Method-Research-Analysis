#!/bin/sh
pwd
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install cmake
cmake --version
sudo apt-get install build-essential
mkdir gromacs/
cd gromacs/
wget https://ftp.gromacs.org/regressiontests/regressiontests-2023.2.tar.gz
tar xvzf regressiontests-2023.2.tar.gz
sudo apt-get install libfftw3-dev
wget https://ftp.gromacs.org/gromacs/gromacs-2023.2.tar.gz
tar xvzf gromacs-2023.2.tar.gz
cd gromacs-2023.2/
mkdir build
cd build
sudo cmake .. -DGMX_BUILD_OWN_FFTW=OFF -DREGRESSIONTEST_DOWNLOAD=OFF -DCMAKE_C_COMPILER=gcc -DREGRESSIONTEST_PATH=./../../regressiontests-2023.2
make check
sudo make install

source /usr/local/gromacs/bin/GMXRC
echo "source /usr/local/gromacs/bin/GMXRC" >> ~/.bashrc

sudo apt install gromacs

gmx pdb2gmx --version
