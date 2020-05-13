#!/bin/bash
cd deps &&

tar -zxf hdf5-1.8.21.tar.gz &&
cd hdf5-1.8.21 &&
./configure --prefix=/usr/local &&
make && make install && cd .. &&

tar -zxf netcdf-c-4.7.4.tar.gz &&
cd netcdf-c-4.7.4 &&
./configure --prefix=/usr/local &&
make && make install && cd .. &&

tar -zxf netcdf-fortran-4.5.2.tar.gz &&
cd netcdf-fortran-4.5.2 &&
./configure --prefix=/usr/local &&
make && make install && cd .. &&

tar -zxf eccodes-2.17.0-Source.gz &&
cd build &&
cmake -DCMAKE_INSTALL_PREFIX=/usr/local -DENABLE_PYTHON=ON -DENABLE_FORTRAN=ON ../eccodes-2.17.0-Source &&
make && make install && cd .. &&
pip3 install --install-option="--prefix=/usr/local" eccodes 
