#F different species

#F1: 
suffix=_5-1_specNO
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/SPECNUM_REL=/c\ SPECNUM_REL=   3," $options_new/COMMAND

#F2 
suffix=_5-2_specAERO-TRACE
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/SPECNUM_REL=/c\ SPECNUM_REL=   25," $options_new/COMMAND

#F3: 
suffix=_5-3_specCO
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/SPECNUM_REL=/c\ SPECNUM_REL=   22," $options_new/COMMAND
output_new=output$suffix

#F4: 
suffix=_5-4_specBC
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/SPECNUM_REL=/c\ SPECNUM_REL=   40," $options_new/COMMAND

#F5: 
suffix=_5-5_bwd_specNO
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/SPECNUM_REL=/c\ SPECNUM_REL=   3," $options_new/COMMAND
sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1," $options_new/COMMAND

