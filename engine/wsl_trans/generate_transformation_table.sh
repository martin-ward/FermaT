#!/bin/sh
# ***********************************************
#
#   File: generate_xml.sh
#
#   Author: Matthias Ladkau
#
#   Description:
#
#     This is a script for generating the XML transformation 
#     list from the FermaT engine source code
#
# ***********************************************
perl generate_transformation_table.pl > transformation_table.xml
