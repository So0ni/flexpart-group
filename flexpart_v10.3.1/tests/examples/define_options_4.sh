#E: unit indices

## forward 
#E1: ind 1 2 
suffix=_4-1_ind_1_2
options_new=options$suffix
cp -r  $options_template $options_new
sed -i '/IND_RECEPTOR=/c\ IND_RECEPTOR=   2,' $options_new/COMMAND

#E2: ind 2 1  
suffix=_4-2_ind_2_1
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/IND_SOURCE=/c\ IND_SOURCE=    2," $options_new/COMMAND

#E3: ind 2 2
suffix=_4-3_ind_2_2
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/IND_SOURCE=/c\ IND_SOURCE=    2," $options_new/COMMAND
sed -i "/IND_RECEPTOR=/c\ IND_RECEPTOR=    2," $options_new/COMMAND


## backward
#E4: bwd 1 2 --   
suffix=_4-4_bwd_ind_1_2
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/IND_RECEPTOR=/c\ IND_RECEPTOR=    2," $options_new/COMMAND
sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1," $options_new/COMMAND

#E5: bwd 2 1 -- 
suffix=_4-5_bwd_ind_2_1
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/IND_SOURCE=/c\ IND_SOURCE=    2," $options_new/COMMAND
sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1," $options_new/COMMAND

#E6: bwd 2 2
suffix=_4-6_bwd_ind_2_2
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/IND_SOURCE=/c\ IND_SOURCE=    2," $options_new/COMMAND
sed -i "/IND_RECEPTOR=/c\ IND_RECEPTOR=    2," $options_new/COMMAND
sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1," $options_new/COMMAND









