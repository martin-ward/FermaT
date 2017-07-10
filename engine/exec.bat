@echo off
rem ***********************************************
rem
rem   File: exec.bat
rem
rem   Author: Matthias Ladkau
rem
rem   Description:
rem
rem     This is a start script for a wsl file 
rem
rem ***********************************************

IF NOT "%1"=="noecho" echo.
IF NOT "%1"=="noecho" echo.
IF NOT "%1"=="noecho" echo FermaT transformation engine
IF NOT "%1"=="noecho" echo ============================

IF NOT "%1"=="noecho" echo fermat_console.bat: Testing for perl ...
perl -e "exit 0" 

IF "%ERRORLEVEL%"=="0" GOTO OK

:ERROR

IF NOT "%1"=="noecho" echo console.bat: Can't find Perl! (Check installation and/or PATH variable)

goto END

:OK

IF NOT "%1"=="noecho" echo fermat_console.bat: Found perl!

IF NOT "%1"=="noecho" echo fermat_console.bat: Changing directory to %~sdp0fermat3\
cd %~sdp0fermat3\

IF NOT "%1"=="noecho" echo fermat_console.bat: Setting the environment variables ...

set FermaT=%~sdp0fermat3
set PATH=%FERMAT%\dosbin;%PATH%
set SCHEME_LIBRARY_PATH=%FERMAT%\slib\
set SCM_INIT_PATH=%FERMAT%\scm\Init5f1.scm
if exist %FERMAT%\scm\slibcat del %FERMAT%\scm\slibcat

IF NOT "%1"=="noecho" echo fermat_console.bat: Executing WSL file ...

IF NOT "%1"=="noecho" echo.
IF NOT "%1"=="noecho" echo.

IF "%1"=="noecho" perl %~sdp0fermat3\bin\wsl %2
IF NOT "%1"=="noecho" perl %~sdp0fermat3\bin\wsl %1

:END
