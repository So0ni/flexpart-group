# J CBLFLAG
# J 1 -- CBLFLAG fwd
suffix=_9-1_CBLFLAG
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/CBLFLAG=/c\ CBLFLAG=   1," $options_new/COMMAND
sed -i "/CTL=/c\ CTL=     10," $options_new/COMMAND
sed -i "/IFINE=/c\ IFINE=  10," $options_new/COMMAND


# J 2 -- CBLFLAG bwd
suffix=_9-2_CBLFLAG_bwd
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/CBLFLAG=/c\ CBLFLAG=   1," $options_new/COMMAND
sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1," $options_new/COMMAND
sed -i "/CTL=/c\ CTL=     10," $options_new/COMMAND
sed -i "/IFINE=/c\ IFINE=  10," $options_new/COMMAND

