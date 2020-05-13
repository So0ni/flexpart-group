#!/bin/bash

# export printgrid=$HOME/repos/flexpart/postproc/flex_read_fortran/printgrid

# default path to examples
#examples_default=./
#examples_default=../../examples
examples_default=../examples

# environment override on default
#examples_int=${1:-$examples_default}
examples_env=${examples:-$examples_default}
#internal (argument override)
examples_int=${1:-$examples_env}




#printgrid_default=printgrid
#printgrid_default=/Users/ignacio/repos/flexpart/postproc/flex_read_fortran/printgrid
printgrid_default=../../postprocess/flex_read_fortran/printgrid
# todo test exixtence of executable
# alternative: printgrid_default=$HOME/repos/flexpart/postprocess/read_fortran/printgrid

#overrun by input from ENV
printgrid_env=${printgrid:-$printgrid_default}
# overrun by command line arguments 
printgrid_int=${2:-$printgrid_env}

echo default example
stdout=$($printgrid_int $examples_int/output/ conc)
echo default $stdout

echo default example variations

#group 1
#declare -a  examples1=('1-1_1' '1-2_2' '1-3_3' '1-4_3')
#declare -a  units1=('conc' 'pptv' 'conc' 'pptv')
#declare -a  examples1=('1-1' '1-2' '1-3' '1-3' '1-5')
declare -a  examples1=('1-1_1' '1-2_2' '1-3_3' '1-3_3' '1-5_5')
declare -a  units1=('conc' 'pptv' 'conc' 'pptv' 'conc')

#group 2
declare -a  examples2=('2-1_bwd' '2-2_bwd5')
declare -a  units2=('time' 'time')




#group 4
declare -a  examples4=('4-1_ind_1_2' '4-2_ind_2_1' '4-3_ind_2_2' '4-4_bwd_ind_1_2' '4-5_bwd_ind_2_1' '4-6_bwd_ind_2_2' )
declare -a  units4=('conc' 'conc' 'conc'  'time' 'time' 'time')

#group 5
declare -a  examples5=('5-1_specNO' '5-2_specAERO-TRACE' '5-3_specCO' '5-4_specBC' '5-5_bwd_specNO' )
declare -a  units5=('conc' 'conc' 'conc'  'conc' 'time')


declare -a  examples6=('6-1_nested' '6-2_nested_bwd' '6-3_nested1' '6-4_nested1_bwd' )
declare -a  units6=('conc' 'time' 'conc'  'time')

declare -a  examples7=('7-1_DOMAINFILL')
declare -a  units7=('conc')


#options_8-1_init_cond/
#options_8-2_init_cond_surf/
#options_8-3_init_cond_ind_2_1/
#options_8-4_init_cond_ind_1_2/
#options_8-5_init_cond_ind_2_2/

declare -a  examples8=('8-1_init_cond' '8-2_init_cond_surf' '8-3_init_cond_ind_2_1' '8-4_init_cond_ind_1_2' '8-5_init_cond_ind_2_2' )
declare -a  units8=('time' 'time' 'time'  'time' 'time')

#output_9-1_CBLFLAG/
#output_9-2_CBLFLAG_bwd/

declare -a  examples9=('9-1_CBLFLAG' '9-2_CBLFLAG_bwd')
declare -a  units9=('conc' 'time')


examples=("${examples1[@]}")
units=("${units1[@]}")

examples=("${examples2[@]}")
units=("${units2[@]}")


examples=("${examples4[@]}")
units=("${units4[@]}")

examples=("${examples5[@]}")
units=("${units5[@]}")


examples=("${examples6[@]}")
units=("${units6[@]}")

examples=("${examples1[@]}" "${examples2[@]}" "${examples4[@]}" "${examples5[@]}" "${examples6[@]}" )
units=("${units1[@]}" "${units2[@]}" "${units4[@]}" "${units5[@]}" "${units6[@]}")

examples=("${examples9[@]}")
units=("${units9[@]}")


examples=("${examples1[@]}" "${examples2[@]}" "${examples4[@]}" "${examples5[@]}" "${examples6[@]}" "${examples7[@]}" "${examples8[@]}" "${examples9[@]}")
units=("${units1[@]}" "${units2[@]}" "${units4[@]}" "${units5[@]}" "${units6[@]}" "${units7[@]}" "${units8[@]}" "${units9[@]}" )




iend=$(( ${#examples[@]} - 1 ))
echo $iend
for i in `seq 0 $iend`;
do 
#echo $i
example=${examples[$i]}
unit=${units[$i]}

#echo $printgrid_int $examples_int/output_${example}/ ${unit}
#stdout=$($printgrid_int $examples_int/output_1-1/ conc)
stdout=$($printgrid_int $examples_int/output_${example}/ ${unit})
#echo $i '&' ${example}: $stdout
echo $i '&' ${example} '&' $stdout '\\'

done

exit
$printgrid_int $examples_int/output_2/ pptv

$printgrid_int $examples_int/output_bwd/ time


exit 
output/
output_1/
output_2/
output_3/
output_4/
output_5/
output_bwd/
output_bwd5/
output_bwd_ind_1_2/
output_bwd_ind_2_1/
output_bwd_ind_2_2/
output_bwd_nc/
output_bwd_specNO/
output_ind_1_2/
output_ind_2_1/
output_ind_2_2/
output_part1/
output_part2/
output_part_bwd1/
output_specAERO-TRACE/
output_specBC/
output_specCO/
output_specNO/
outputpart_QUASILAG/


