#!/bin/bash
# loop on list of option dirs 
# extract suffixes 
# generate corresponding pathnames from working template
# make corresponding output dirs locally

# snippet from 
# /Users/ignacio/todo/doing/flexpart/2018-05-25/loop_on_ls.sh
# /Users/ignacio/todo/doing/flexpart/2018-05-24/loop_on_ls.sh
# there the aim was to convert the format

#pathnames_template=~/repos/flexpart/pathnames
pathnames_template=./pathnames
echo pathnames_template = $pathnames_template 

#defaultdir=
#echo $defaultdir

#OUTPUT="$(ls -d1 ${1:-$defaultdir}options*)"
#option_dir_list="$(ls -d1 options*)"
#option_dir_list=${1:-"$(ls -d1 options*)"}
option_dir_list="$(ls -d1 options${1}*)"


for i in ${option_dir_list}
do
  # echo $i
  # echo expand:
  # extract suffix from options dirs
  suffix=${i#*options}
  echo suffix=$suffix
  options_new=$i

  # define name of new output and create it
  output_new=output$suffix
  if [ -d $output_new ]; then
     mv $output_new ${output_new}.bk 
  fi 
  mkdir $output_new

  # define name of new pathnames
  pathnames_new=pathnames$suffix
  echo pathnames_new = $pathnames_new
  # copy template (contains valid AVAILABLE file and winds)
  cp $pathnames_template $pathnames_new
  # replace options line
  sed -i "s/\boptions\b/$options_new/g" $pathnames_new
  # replace output line
  sed -i "s/\boutput\b/$output_new/g" $pathnames_new
done

