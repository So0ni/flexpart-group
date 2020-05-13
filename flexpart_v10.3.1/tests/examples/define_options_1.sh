suffix=_1-1_1
options_new=options$suffix
cp -r  $options_template $options_new
sed -i '/IOUT=/c\ IOUT=1,' $options_new/COMMAND

suffix=_1-2_2
options_new=options$suffix
cp -r  $options_template $options_new
sed -i '/IOUT=/c\ IOUT=2,' $options_new/COMMAND

suffix=_1-3_3
options_new=options$suffix
cp -r  $options_template $options_new
sed -i '/IOUT=/c\ IOUT=3,' $options_new/COMMAND

suffix=_1-4_4
options_new=options$suffix
cp -r  $options_template $options_new
sed -i '/IOUT=/c\ IOUT=4,' $options_new/COMMAND

suffix=_1-5_5
options_new=options$suffix
cp -r  $options_template $options_new
sed -i '/IOUT=/c\ IOUT=5,' $options_new/COMMAND

suffix=_1-6_9
options_new=options$suffix
cp -r  $options_template $options_new
sed -i '/IOUT=/c\ IOUT=9,' $options_new/COMMAND

suffix=_1-7_10
options_new=options$suffix
cp -r  $options_template $options_new
sed -i '/IOUT=/c\ IOUT=10,' $options_new/COMMAND

suffix=_1-8_11
options_new=options$suffix
cp -r  $options_template $options_new
sed -i '/IOUT=/c\ IOUT=11,' $options_new/COMMAND

suffix=_1-9_12
options_new=options$suffix
cp -r  $options_template $options_new
sed -i '/IOUT=/c\ IOUT=12,' $options_new/COMMAND



