#!/bin/bash

define_options_dir_list="$(ls -d1 define_options*)"

for i in ${define_options_dir_list}
do
  echo call ./gen_options.sh $i
  ./gen_options.sh $i
done
