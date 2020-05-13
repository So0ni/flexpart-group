#!/bin/bash
# runs the definitions $define_options with template $options_template
# options_template is a working options dir, by default from the repo HEAD
# $define_options must contain a suffix (empty for default) and sed substitutions

define_options_default=define_options_default.sh
#define_options=${1:-$define_options_default}
define_options=${1:-$define_options_default}


#template is in 1 or defalt here
#options_template_default=~/repos/flexpart/options
#options_template_default=~/repos/flex_gen_input/reference/options
# relative path, options reference copied 
#options_template_default=./options
# relative path to be included in the distribution
options_template_default=./options

options_template=${2:-$options_template_default}

# to do: 
# source settings

#define options contains $options_template 
source $define_options

