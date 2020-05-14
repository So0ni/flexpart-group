#!/bin/bash

# export ECCODES_DIR=/usr/local
# export LD_LIBRARY_PATH=/usr/local/lib
# export GRIB_API_INCLUDE_DIR=/usr/local/include
# export GRIB_API_LIB="-L/usr/local/lib -Bstatic -lgrib_api_f77 -lgrib_api_f90 -lgrib_api -Bdynamic -lm -ljasper"
# export EMOSLIB=-lemosR64

echo "export ECCODES_DIR=/usr/local" >> /etc/profile &&
echo "export LD_LIBRARY_PATH=/usr/local/lib" >> /etc/profile &&
echo "export GRIB_API_INCLUDE_DIR=/usr/local/include" >> /etc/profile &&
echo 'export GRIB_API_LIB="-L/usr/local/lib -Bstatic -lgrib_api_f77 -lgrib_api_f90 -lgrib_api -Bdynamic -lm -ljasper"' >> /etc/profile &&
echo "export EMOSLIB=-lemosR64" >> /etc/profile
source /etc/profile &&

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

tar -zxf grib_api-1.28.0-Source.tar.gz &&
cd grib_api-1.28.0-Source &&
./configure --prefix=/usr/local --enable-python &&
make && make install && cd .. &&

tar -zxf eccodes-2.17.0-Source.tar.gz &&
cd eccodes-build &&
cmake -DCMAKE_INSTALL_PREFIX=/usr/local -DENABLE_PYTHON=ON -DENABLE_FORTRAN=ON -DBUILD_SHARED_LIBS=BOTH -DCMAKE_C_FLAGS="-fPIC" -DCMAKE_Fortran_FLAGS="-fPIC" ../eccodes-2.17.0-Source &&
make && make test && make install && cd .. &&

pip3 install eccodes &&

tar -zxf fftw-3.3.8.tar.gz &&
cd fftw-3.3.8 &&
./configure --prefix=/usr/local --enable-shared &&
make && make install && cd .. &&

cd libemos-build &&
cmake -DENABLE_GRIBEX_ABORT=OFF ../libemos-4.5.9-Source &&
make && make test && make install && cd .. &&

cd .. &&
wget https://www.flexpart.eu/downloads/66 -O flexpart_v10.4.tar &&
tar -xf flexpart_v10.4.tar && mv flexpart_v10.4_3d7eebf flexpart_v10.4 &&

cd flexpart_v10.4/src && 
sed -i 's/INCPATH1  = \${ROOT_DIR}\/gcc-5.4.0\/include/INCPATH1  = \${ROOT_DIR}\/include/g' makefile &&
sed -i 's/LIBPATH1 = \${ROOT_DIR}\/gcc-5.4.0\/lib/LIBPATH1 = \${ROOT_DIR}\/lib/g' makefile &&
make serial ncf=yes  &&

cd ../preprocess/flex_extract/src  && mv Makefile.gfortran Makefile && make &&
cd ../python &&
sed -i 's/from gribapi import \*/from grib_api import \*/g' GribTools.py &&
sed -i 's/from gribapi import \*/from grib_api import \*/g' FlexpartTools.py &&
sed -i 's/from gribapi import \*/from grib_api import \*/g' opposite.py &&
 
pip3 install ecmwf-api-client &&
pip install ecmwf-api-client

