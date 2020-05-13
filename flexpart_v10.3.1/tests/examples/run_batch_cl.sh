#!/bin/bash

OUTPUTdefault="$(ls -1 *.cl)"
#OUTPUT=${OUTPUTvar:-$OUTPUTdefault}
# could input a subset of jobs to run "$(ls -1 *.cl)"
OUTPUT=${1:-$OUTPUTdefault}

#rm joblist

for i in ${OUTPUT}
do
   echo batch job $i #| tee -a joblist1$group
   #source $i | tee -a joblist2$group
   source $i | tee ${i}.stdout  # -a joblist2$group
   echo '##########################################' 
done



