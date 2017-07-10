#!/bin/sh
# ***********************************************
#
#   File: console.bat
#
#   Author: Matthias Ladkau
#
#   Description:
#
#     This is a start script for the 
#     FermaT transformation engine console
#
# ***********************************************

if [ ! "$1" = "noecho" ]
then
echo
echo
echo "FermaT transformation engine console"
echo "===================================="
echo "fermat_console.sh: Testing for perl ..."
fi

if ( ! perl -e "exit 0" )
then
echo "fermat_console.sh: Can't find Perl! (Check installation and/or PATH variable)"
exit 1
fi

if [ ! "$1" = "noecho" ]
then
echo "fermat_console.sh: Found perl!"
echo "fermat_console.sh: Changing directory to $PWD/engine/fermat3"
fi

cd $PWD/engine/fermat3

if [ ! "$1" = "noecho" ]
then
echo "fermat_console.sh: Setting the environment variables ..."
fi

FermaT=$PWD
export FermaT
PATH="$FermaT/bin":$PATH
export PATH

SCHEME_LIBRARY_PATH="$FermaT/slib/"
export SCHEME_LIBRARY_PATH
SCM_INIT_PATH="$FermaT/scm/Init5f1.scm"
export SCM_INIT_PATH

./`uname`/scmfmt

