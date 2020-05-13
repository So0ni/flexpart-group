#/bin/bash

#lev=../

lev_default=../../

#overrun by input from ENV
lev_env=${lev:-$lev_default}
# overrun by command line arguments 
lev_int=${1:-$lev_env}




#cp -r ../options_flex_ecmwf_EA options
#cp -r ${lev}options_flex_ecmwf_EA options

if [ -d options ]; then
  mv options options.bk
fi

echo cp -r ${lev_int}options options
cp -r ${lev_int}options options


#cp ../pathnames_laptop_EA pathnames
#flex_winds=../flex_winds/ECMWF/work/
#./gen_pathnames_default.sh ../AVAILABLE_flex_ecmwf_EA 
#./gen_pathnames_default.sh ${lev}AVAILABLE_flex_ecmwf_EA ${lev}flex_extract/work/

echo ./gen_pathnames_default.sh ${lev_int}AVAILABLE ${lev_int}preprocess/flex_extract/work/
./gen_pathnames_default.sh ${lev_int}AVAILABLE ${lev_int}preprocess/flex_extract/work/


if [ ! -d output ]; then
   echo mkdir output
   mkdir output

fi





