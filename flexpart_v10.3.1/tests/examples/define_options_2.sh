#C: backward runs: bwd, bwd5, bwd_nc
#rm -r  *_bwd
#C1: bwd -- default (conc)

suffix=_2-1_bwd
options_new=options$suffix
cp -r  $options_template $options_new
sed -i '/LDIRECT=/c\ LDIRECT=    -1,' $options_new/COMMAND
sed -i '/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1,' $options_new/COMMAND
sed -i '/IOUT=/c\ IOUT=    1,' $options_new/COMMAND #should not be needed

#C2: bwd5 -- bwd cluster 
suffix=_2-2_bwd5
options_new=options$suffix
#options_new=options_bwd5
cp -r  $options_template $options_new
sed -i '/LDIRECT=/c\ LDIRECT=    -1,' $options_new/COMMAND
sed -i '/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1,' $options_new/COMMAND
sed -i '/IOUT=/c\ IOUT=    5,' $options_new/COMMAND

#C3: bwd_nc -- bwd netCDF
suffix=_2-3_bwd_nc
options_new=options$suffix
cp -r  $options_template $options_new
sed -i '/LDIRECT=/c\ LDIRECT=    -1,' $options_new/COMMAND
sed -i '/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1,' $options_new/COMMAND
sed -i '/IOUT=/c\ IOUT=    9,' $options_new/COMMAND
