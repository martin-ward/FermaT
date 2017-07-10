#!/bin/sh
# ***********************************************
#
#   File: metrics.sh
#
#   Author: Matthias Ladkau
#
#   Description:
#
#    Compute metrics for a wsl file 
#
# ***********************************************

if [ ! "$1" = "noecho" ]
then
echo
echo
echo "FermaT transformation engine console"
echo "===================================="
echo "metrics.sh: Testing for perl ..."
fi

if ( ! perl -e "exit 0" )
then
echo "metrics.sh: Can't find Perl! (Check installation and/or PATH variable)"
exit 1
fi

if [ ! "$1" = "noecho" ]
then
echo "metrics.sh: Found perl!"
echo "metrics.sh: Changing directory to $PWD/engine/fermat3"
fi

cd $PWD/engine/fermat3

if [ ! "$1" = "noecho" ]
then
echo "metrics.sh: Setting the environment variables ..."
fi

FermaT=$PWD
export FermaT
PATH="$FermaT/bin":$PATH
export PATH

SCHEME_LIBRARY_PATH="$FermaT/slib/"
export SCHEME_LIBRARY_PATH
SCM_INIT_PATH="$FermaT/scm/Init5f1.scm"
export SCM_INIT_PATH

if [ "$1" = "noecho" ]
then
perl ./bin/metrics $2
else
perl ./bin/metrics $1
fi

