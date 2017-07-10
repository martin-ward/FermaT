@echo off
rem ***********************************************
rem
rem   File: metrics.bat
rem
rem   Author: Martin Ward
rem
rem   Description:
rem
rem     Compute metrics for a wsl file 
rem
rem ***********************************************

IF NOT "%1"=="noecho" echo.
IF NOT "%1"=="noecho" echo.
IF NOT "%1"=="noecho" echo FermaT transformation engine
IF NOT "%1"=="noecho" echo ============================

IF NOT "%1"=="noecho" echo metrics.bat: Testing for perl ...
perl -e "exit 0" 

IF "%ERRORLEVEL%"=="0" GOTO OK

:ERROR

IF NOT "%1"=="noecho" echo metrics.bat: Can't find Perl! (Check installation and/or PATH variable)

goto END

:OK

IF NOT "%1"=="noecho" echo metrics.bat: Found perl!

IF NOT "%1"=="noecho" echo metrics.bat: Changing directory to %~sdp0fermat3\
cd %~sdp0fermat3\

IF NOT "%1"=="noecho" echo metrics.bat: Setting the environment variables ...

set FermaT=%~sdp0fermat3
set PATH=%FERMAT%\dosbin;%PATH%
set SCHEME_LIBRARY_PATH=%FERMAT%\slib\
set SCM_INIT_PATH=%FERMAT%\scm\Init5f1.scm
if exist %FERMAT%\scm\slibcat del %FERMAT%\scm\slibcat

IF NOT "%1"=="noecho" echo metrics.bat: Executing WSL file ...

IF NOT "%1"=="noecho" echo.
IF NOT "%1"=="noecho" echo.

IF "%1"=="noecho" perl %~sdp0fermat3\bin\metrics %2
IF NOT "%1"=="noecho" perl %~sdp0fermat3\bin\metrics %1

:END
