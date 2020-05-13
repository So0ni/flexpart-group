#!/bin/sh
# (C) Copyright 2005- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities granted to it by
# virtue of its status as an intergovernmental organisation nor does it submit to any jurisdiction.

. ./include.sh

#Define a common label for all the tmp files
label="bufr_clone_test_p"

#Prepare tmp file
fBufrTmp=${label}.clone.bufr
rm -f $fBufrTmp | true

#We clone a bufr file with multiple messages
f=${data_dir}/bufr/syno_1.bufr

REDIRECT=/dev/null

#The input ($f) and output ($fBufrTmp) are hardcoded in the example!!!  
$PYTHON $examples_src/bufr_clone.py >$REDIRECT 2> $REDIRECT 

#TODO: add more checks

#Compare clone to the original
set +e
${tools_dir}/bufr_compare $f $fBufrTmp >$REDIRECT 2> $REDIRECT 

#Check if clone is different
if [ $? -eq 0 ]; then
   echo "cloning produced identical files " >&2
   exit 1
fi

set -e

#Clean up
rm -f ${fBufrTmp}

