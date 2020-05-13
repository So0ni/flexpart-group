#!/bin/bash
# Ignacio Pisso 4/2018
# modified 30/10/18
# change default executable to  "../../FLEXPART". IP 12/2018
# optional args: 
# 1) flexpart executable (default "../../FLEXPART")
# 2) FLEXPART inline arguments (-i, -v, etc) (default none)
# example gen_batch_jobs_cl.sh FLEXPART -i

OUTPUTdefault="$(ls -1 pathnames*)"
OUTPUT=${OUTPUTvar:-$OUTPUTdefault}
#OUTPUTpwd="$(pwd)"



#FLEXPARTdefault=FLEXPART_8d70e43
# if FLEXPART is in $PATH
FLEXPARTdefault=../../src/FLEXPART

FLEXPART_env=${FLEXPART:-$FLEXPARTdefault}

#FLEXPART=${FLEXPARTvar:-$FLEXPARTdefault}
# $FLEXPART is the command line in or default defined above
#FLEXPART=${1:-$FLEXPARTdefault}
FLEXPART=${1:-$FLEXPART_env}

argsdefault=' ' # blank string
# if 2nd arg
args=${2:-$argsdefault}

# echo $OUTPUT

for i in ${OUTPUT}
do

suffix=${i#*patnames}

#FP_slurm_batch_sl=slurm_batch_$i.sl
#FP_slurm_batch_sl=batch_dry_$i.sl
#FP_slurm_batch_sl=clrun_full_$i.sl
#FP_slurm_batch_sl=fork_cl_$i.sl
FP_slurm_batch_sl=batch_job_$suffix.cl

#        echo  '                                          ' >> $FP_slurm_batch_sl
        echo  '                                          ' > $FP_slurm_batch_sl
        #FLEXPART_run_string="$FLEXPART  $i -i"
        FLEXPART_run_string="$FLEXPART  $i $args"
        echo $FLEXPART_run_string   >> $FP_slurm_batch_sl
	echo $FLEXPART_run_string
done

