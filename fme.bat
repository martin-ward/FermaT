@echo off
rem ***********************************************
rem
rem   File: fme.bat
rem
rem   Author: Matthias Ladkau
rem
rem   Description:
rem
rem     This is a start script for the 
rem     FermaT Maintenance Environment
rem
rem ***********************************************

cd %~sdp0

start /B javaw -classpath ./lib/fme.jar;./lib/ave.jar;./lib/toniclf.jar;./lib/substance.jar;./lib/dynamic_typed_wsl.jar;./lib/simple_typed_wsl.jar;./lib/weak_unsafe_typed_wsl.jar;./lib/strong_unsafe_typed_wsl.jar;./lib/xercesImpl.jar;./lib/xmlParserAPIs.jar;./lib/stringsearch.jar;./lib/sve.jar;./lib/zvtm.jar fme.FMEMain
:END
