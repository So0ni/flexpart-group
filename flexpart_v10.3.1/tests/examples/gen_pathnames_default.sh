#!/bin/bash

#hardcoded defaults
options_default=./options/
output_default=./output/
flex_winds_default=./flex_extract/work/ #blank 
AVAILABLE_default=./AVAILABLE

# flex_winds=${1:-"../flex_extract/work/"}
# AVAILABLE=${2:-"../AVAILABLE_flex_ecmwf_EA"}

#overrun by input from ENV
options_env=${options:-$options_default}
output_env=${output:-$output_default}
flex_winds_env=${flex_winds:-$flex_winds_default}
AVAILABLE_env=${AVAILABLE:-$AVAILABLE_default}

# overrun by command line arguments 
options_pathnames=${3:-$options_env}
output_pathnames=${4:-$output_env}
flex_winds_pathnames=${2:-$flex_winds_env}
AVAILABLE_pathnames=${1:-$AVAILABLE_env} 

# generate output 
echo $options_pathnames > pathnames
echo $output_pathnames >> pathnames
echo $flex_winds_pathnames >> pathnames
echo $AVAILABLE_pathnames >> pathnames

