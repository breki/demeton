SET CONFIG=Release
SET BUILD_DIR=builds\Demeton.Console
SET SRTM_CACHE="%cd%\samples\cache"

rm -r "%BUILD_DIR%"

rem dotnet clean --configuration %CONFIG%
dotnet build --configuration %CONFIG% --verbosity minimal --no-incremental
if %errorlevel% neq 0 exit /b %errorlevel%

dotnet test --configuration %CONFIG% --verbosity minimal ^
	--filter Category!=acceptance
if %errorlevel% neq 0 exit /b %errorlevel%

dotnet publish Demeton.Console -c %CONFIG% --output "%cd%\%BUILD_DIR%"
if %errorlevel% neq 0 exit /b %errorlevel%

