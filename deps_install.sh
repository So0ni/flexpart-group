#!/bin/bash

cd deps/hdf5-1.8.21 &&
chmod +x configure &&
./configure --prefix=/usr/local &&
make && make install &&

cd ../netcdf-c-4.7.4 &&
chmod +x configure &&
./configure --prefix=/usr/local &&
make && make install &&

cd ../netcdf-fortran-4.5.2 &&
chmod +x configure &&
./configure --prefix=/usr/local &&
make && make install &&

cd ../build &&
cmake -DCMAKE_INSTALL_PREFIX=/usr/local -DENABLE_PYTHON=ON -DENABLE_FORTRAN=ON ../eccodes-2.17.0-Source &&
make && make install &&
pip3 install --install-option="--prefix=/usr/local" eccodes 
