#D: particle output
#D1: part1: Trajectories from default (conc)
#D2: part2 -- default + partposit in the end
#D3: part_bwd1 --  backward trajectories 
#D4: part_QUASILAG: Trajectories MQUASILAG


#D1: part1: Trajectories from default (conc)
suffix=_3-part1
#options_new=options_part1
options_new=options$suffix
cp -r  $options_template $options_new
sed -i '/IPOUT=/c\ IPOUT=    1,' $options_new/COMMAND

#D2: part2 -- default + partposit in the end
suffix=_3-part2
options_new=options$suffix
#options_new=options_part2
cp -r  $options_template $options_new
sed -i '/IPOUT=/c\ IPOUT=    2,' $options_new/COMMAND

#D3: part_bwd1 --  backward trajectories 
suffix=_3-part_bwd1
options_new=options$suffix
#options_new=options_part_bwd1
cp -r  $options_template $options_new
sed -i '/LDIRECT=/c\ LDIRECT=    -1,' $options_new/COMMAND
sed -i '/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    1,' $options_new/COMMAND
sed -i '/IPOUT=/c\ IPOUT=    1,' $options_new/COMMAND

#D4: part_QUASILAG: Trajectories MQUASILAG
suffix=_3-part_QUASILAG
options_new=options$suffix
cp -r  $options_template $options_new
sed -i '/MQUASILAG=/c\ MQUASILAG=    1,' $options_new/COMMAND
sed -i '/IOUTPUTFOREACHRELEASE=/c\ IOUTPUTFOREACHRELEASE=    0,' $options_new/COMMAND

