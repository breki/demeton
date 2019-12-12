@ECHO off

SET CONFIG=Release
SET BUILD_DIR=builds\Demeton.Console
SET SRTM_CACHE="%cd%\samples\cache"

rm -r "%BUILD_DIR%"

rem dotnet clean --configuration %CONFIG%

ECHO.
ECHO BUILDING THE CODE...

dotnet build --configuration %CONFIG% --verbosity minimal --no-incremental
if %errorlevel% neq 0 exit /b %errorlevel%

ECHO.
ECHO RUNNING THE TESTS...

dotnet test --configuration %CONFIG% --verbosity minimal ^
	--filter Category!=acceptance
if %errorlevel% neq 0 exit /b %errorlevel%

ECHO RUNNING THE F# LINT...

dotnet tool install --tool-path tools\fsharplint dotnet-fsharplint
tools\fsharplint\dotnet-fsharplint.exe -sol Demeton.sln
if %errorlevel% neq 0 exit /b %errorlevel%

ECHO.
ECHO MAKING THE CONSOLE PACKAGE...

dotnet publish Demeton.Console -c %CONFIG% --output "%cd%\%BUILD_DIR%"
if %errorlevel% neq 0 exit /b %errorlevel%

ECHO.
ECHO BUILD SUCCESSFUL
