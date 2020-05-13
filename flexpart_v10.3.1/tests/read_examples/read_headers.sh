#!/bin/bash

echo usage ./read_headers.sh $examples printheader

echo to work from '$flexhome/tests/read_examples/' 
#examples_default=./
examples_default=../examples


examples_env=${examples:-$examples_default}
examples_int=${1:-$examples_env}


OUTPUTdefault="$(ls -1d $examples_int/output*)"

#OUTPUT=${2:-$OUTPUTdefault}
OUTPUT_int=${OUTPUT:-$OUTPUTdefault}

#printheader_default=printheader
# printheader=$HOME/repos/flexpart/postprocess/read_fortran/printheader
# printheader_default=$HOME/repos/flexpart/postprocess/read_fortran/printheader
printheader_default=../../postprocess/flex_read_fortran/printheader


echo defaults:
echo '$examples=' $examples_default
echo '$printheader=' $printheader_default


#overrun by input from ENV
printheader_env=${printheader:-$printheader_default}
# overrun by command line arguments 
printheader_int=${2:-$printheader_env}

echo printheader_int $printheader_int 
echo  

for i in ${OUTPUT_int}
do
   echo print header for $i 
   #echo cl: $printheader_int $examples_int/$i/  
   echo cl: $printheader_int $i/  

   $printheader_int $i/ 

done




