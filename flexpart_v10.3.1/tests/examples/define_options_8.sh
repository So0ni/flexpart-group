# I  LINIT_COND + SURF_ONLY
# I1: Gmode
#suffix=_8-1_Gmode_bwd
suffix=_8-1_init_cond
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/LINIT_COND=/c\ LINIT_COND=   1," $options_new/COMMAND
sed -i "/SURF_ONLY=/c\ SURF_ONLY=   1," $options_new/COMMAND
sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1," $options_new/COMMAND

# I2: Gmode_bwd
suffix=_8-2_init_cond_surf
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/LINIT_COND=/c\ LINIT_COND=   1," $options_new/COMMAND
sed -i "/SURF_ONLY=/c\ SURF_ONLY=   1," $options_new/COMMAND
sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1," $options_new/COMMAND

# I2: Gmode_bwd
suffix=_8-3_init_cond_ind_2_1
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/LINIT_COND=/c\ LINIT_COND=   1," $options_new/COMMAND
sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1," $options_new/COMMAND
sed -i '/IND_RECEPTOR=/c\ IND_RECEPTOR=   2,' $options_new/COMMAND


suffix=_8-4_init_cond_ind_1_2
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/LINIT_COND=/c\ LINIT_COND=   1," $options_new/COMMAND
sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1," $options_new/COMMAND
sed -i '/IND_RECEPTOR=/c\ IND_RECEPTOR=   2,' $options_new/COMMAND
sed -i "/IND_SOURCE=/c\ IND_SOURCE=    2," $options_new/COMMAND
#sed -i "/IND_SOURCE=/c\ IND_SOURCE=    2," $options_new/COMMAND


suffix=_8-5_init_cond_ind_2_2
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/LINIT_COND=/c\ LINIT_COND=   1," $options_new/COMMAND
sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1," $options_new/COMMAND
sed -i '/IND_RECEPTOR=/c\ IND_RECEPTOR=   2,' $options_new/COMMAND
sed -i "/IND_SOURCE=/c\ IND_SOURCE=    2," $options_new/COMMAND


