#!/bin/bash

echo  compare two default examples
echo  'USAGE: ./compare_grids.sh $reference $examples $flex_read_compare2' 
# 

# 1st optional argument path to reference outputs
#default_reference=../cases_EA_ACEF 
#default_reference=../../examples/ 
# default_reference=./examples/ 
default_reference=./examples_reference/ 


old=${1:-$default_reference}

#default_cases=./
#default_cases=../examples2/
default_cases=./examples_bis/
default_cases=./examples_reference/
#new=${3:-$default_cases}
new=${2:-$default_cases}

# 3rd optional argument: path to flex_read_compare2 (default in the path) 

flex_read_compare2_default=../postprocess/read_fortran/flex_read_compare2  #flex_read_compare2
#overrun by input from ENV
flex_read_compare2_env=${flex_read_compare2:-$flex_read_compare2_default}
# overrun by command line arguments 
flex_read_compare2_int=${3:-$flex_read_compare2_env}


#default_flexcompare='flex_read_compare2'
flex_compare_grids=$flex_read_compare2_int  #${2:-$default_flexcompare}

# source from file



#declare -a  examples1=('1-1_1' '1-2_2' '1-3_3' '1-3_3' '1-5_5')
#declare -a  units1=('conc' 'pptv' 'conc' 'pptv' 'conc')


#examples=("${examples1[@]}")
#units=("${units1[@]}")

source declare_examples


iend=$(( ${#examples[@]} - 1 ))
echo '#examples' = $iend
echo old: $old/
echo new: $new
echo flex_compare_grids: $flex_compare_grids 

for i in `seq 0 $iend`;
do 
#echo i=$i
example=${examples[$i]}
unit=${units[$i]}

#echo $example
#echo $unit

#stdout=$($printgrid_int $examples_int/output_${example}/ ${unit})
#echo $i '&' ${example} '&' $stdout '\\'

#output=output_1 ; unit=conc 
output=output_$example
#echo true: output_1-1_1

##echo $old/$output/
#echo $new$output/ 
$flex_compare_grids  $old/$output/ $new$output/ $unit > stdout.${output}_${unit}
echo $(($i+1)) example $example ${unit} $? 



done



exit


if [[ $? -ne 0 ]]; then
  exit $?
fi

output=output_1 ; unit=conc 
$flex_compare_grids  $old/$output/ $new$output/ $unit > stdout.${output}_${unit}
echo $output ${unit} $? 

output=output_2 ; unit=pptv 
$flex_compare_grids  $old/$output/ $new$output/ $unit > stdout.${output}_${unit}
echo $output ${unit} $? 

output=output_3 ; unit=conc 
$flex_compare_grids  $old/$output/ $new$output/ $unit > stdout.${output}_${unit}
echo $output ${unit} $? 

#clusters
echo ./flex_traj_compare

# backward
output=output_bwd ; unit=time 
$flex_compare_grids  $old/$output/ $new$output/ $unit > stdout.${output}_${unit}
echo $output ${unit} $? 


#indices
output=output_ind_1_2 ; unit=conc 
$flex_compare_grids  $old/$output/ $new$output/ $unit > stdout.${output}_${unit}
echo $output ${unit} $? 

output=output_ind_2_1 ; unit=conc 
$flex_compare_grids  $old/$output/ $new$output/ $unit > stdout.${output}_${unit}
echo $output ${unit} $? 

output=output_ind_2_2 ; unit=conc 
$flex_compare_grids  $old/$output/ $new$output/ $unit > stdout.${output}_${unit}
echo $output ${unit} $? 

#fail
#output=output_ind_2_2 ; unit=conc 
#$flex_compare_grids  $old/$output/ output_ind_2_1/ $unit > stdout.${output}_${unit}
#fail=$? 
#echo $output ${unit} $fail 

if [[ $fail -ne 0 ]]; then
  exit $fail
fi



output=output_specAERO-TRACE ; unit=conc 
$flex_compare_grids  $old/$output/ $new$output/ $unit > stdout.${output}_${unit}
echo $output ${unit} $? 

output=output_specBC ; unit=conc 
$flex_compare_grids  $old/$output/ $new$output/ $unit > stdout.${output}_${unit}
echo $output ${unit} $? 

output=output_specCO ; unit=conc 
$flex_compare_grids  $old/$output/ $new$output/ $unit > stdout.${output}_${unit}
echo $output ${unit} $? 

output=output_specNO ; unit=conc 
$flex_compare_grids  $old/$output/ $new$output/ $unit > stdout.${output}_${unit}
echo $output ${unit} $? 

if [[ $fail -ne 0 ]]; then
  exit $fail
fi



