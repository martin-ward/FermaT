@echo off
rem ***********************************************
rem
rem   File: generate_xml.bat
rem
rem   Author: Matthias Ladkau
rem
rem   Description:
rem
rem     This is a script for generating the XML transformation 
rem     list from the FermaT engine source code
rem
rem ***********************************************
cd %~ds0
perl generate_transformation_table.pl > transformation_table.xml
