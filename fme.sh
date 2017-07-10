#!/bin/sh
# ***********************************************
#
#   File: fme.sh
#
#   Author: Matthias Ladkau
#
#   Description:
#
#     This is a start script for the 
#     FermaT Maintenance Environment
#
# ***********************************************

cd `dirname $0`
java=${JAVA-java}

$java -classpath ./lib/fme.jar:./lib/ave.jar:./lib/toniclf.jar:./lib/substance.jar:./lib/dynamic_typed_wsl.jar:./lib/simple_typed_wsl.jar:./lib/weak_unsafe_typed_wsl.jar:./lib/strong_unsafe_typed_wsl.jar:./lib/xercesImpl.jar:./lib/xmlParserAPIs.jar:./lib/stringsearch.jar:./lib/sve.jar:./lib/zvtm.jar fme.FMEMain
