suffix=_10-1_bwd_ind_1_3
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/IND_RECEPTOR=/c\ IND_RECEPTOR=    3," $options_new/COMMAND
#sed -i "/IND_SOURCE=/c\ IND_SOURCE=    3," $options_new/COMMAND
sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1," $options_new/COMMAND

suffix=_10-2_bwd_ind_1_4
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/IND_RECEPTOR=/c\ IND_RECEPTOR=    4," $options_new/COMMAND
#sed -i "/IND_SOURCE=/c\ IND_SOURCE=    4," $options_new/COMMAND
sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1," $options_new/COMMAND

suffix=_10-3_bwd_ind_2_3
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/IND_RECEPTOR=/c\ IND_RECEPTOR=    3," $options_new/COMMAND
sed -i "/IND_SOURCE=/c\ IND_SOURCE=    2," $options_new/COMMAND
sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1," $options_new/COMMAND

suffix=_10-4_bwd_ind_2_4
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/IND_RECEPTOR=/c\ IND_RECEPTOR=    4," $options_new/COMMAND
sed -i "/IND_SOURCE=/c\ IND_SOURCE=    2," $options_new/COMMAND
sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1," $options_new/COMMAND





