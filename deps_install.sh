#!/bin/bash
cd deps &&

tar -zxf jasper-1.900.2.tar.gz &&
cd jasper-1.900.2 &&
./configure --prefix=/usr/local &&
make && make install && cd .. &&

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

tar grib_api-1.28.0-Source.tar.gz &&
cd grib_api-1.28.0-Source &&
./configure --prefix=/usr/local &&
make && make install && cd .. &&

tar -zxf eccodes-2.17.0-Source.tar.gz &&
cd build &&
cmake -DCMAKE_INSTALL_PREFIX=/usr/local -DENABLE_PYTHON=ON -DENABLE_FORTRAN=ON -DBUILD_SHARED_LIBS=BOTH -DCMAKE_C_FLAGS="-fPIC" -DCMAKE_Fortran_FLAGS="-fPIC" ../eccodes-2.17.0-Source &&
make && make test && make install && cd .. &&
pip3 install eccodes && cd .. &&

wget https://www.flexpart.eu/downloads/66 -O flexpart_v10.4.tar &&
tar -xf flexpart_v10.4.tar && mv flexpart_v10.4_3d7eebf flexpart_v10.4 &&

cd flexpart_v10.4/src && make serial ncf=yes
