# H  MDOMAINFILL
# H1: DOMAINFILL
suffix=_7-1_DOMAINFILL
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/MDOMAINFILL=/c\ MDOMAINFILL=   1," $options_new/COMMAND
#sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    0," $options_new/COMMAND

 #### FLEXPART MODEL ERROR! FILE COMMAND:     ####
 #### FOR DOMAIN FILLING RUNS OUTPUT FOR      ####
 #### EACH RELEASE IS FORBIDDEN !             ####
##########################################

suffix=_7-2_DOMAINFILL_bwd
options_new=options$suffix
cp -r  $options_template $options_new
sed -i "/MDOMAINFILL=/c\ MDOMAINFILL=   1," $options_new/COMMAND
sed -i "/LDIRECT=/c\ LDIRECT=    -1," $options_new/COMMAND
sed -i "/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    0," $options_new/COMMAND


 #### FLEXPART MODEL ERROR! FILE COMMAND:     ####
 #### FOR BACKWARD RUNS, IOUTPUTFOREACHRLEASE ####
 #### MUST BE SET TO ONE!                     ####
##########################################



