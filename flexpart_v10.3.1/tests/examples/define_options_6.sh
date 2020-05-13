#G nested output

#G1
suffix=_6-1_nested
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/NESTED_OUTPUT=/c\ NESTED_OUTPUT=   1," $options_new/COMMAND

#G2
suffix=_6-2_nested_bwd
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/NESTED_OUTPUT=/c\ NESTED_OUTPUT=   1," $options_new/COMMAND
sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1," $options_new/COMMAND


#G1
suffix=_6-3_nested1
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/NESTED_OUTPUT=/c\ NESTED_OUTPUT=   1," $options_new/COMMAND
sed -i "/LON1=/c\ LON1=  -0.1," $options_new/RELEASES
sed -i "/LON1=/c\ LON1=   0.1," $options_new/RELEASES
sed -i "/OUTLAT0=/c\ OUTLAT0=  -3," $options_new/OUTGRID_NEST



# LON1=    0.000    ,
# LON2=    0.000    ,
# LAT1=   0.000    ,
# LAT2=   0.000    ,
# Z1=    50.000       ,
# Z2=    50.000       ,

#G2
suffix=_6-4_nested1_bwd
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/NESTED_OUTPUT=/c\ NESTED_OUTPUT=   1," $options_new/COMMAND
sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1," $options_new/COMMAND

