SET BUILD_DIR=builds\Demeton.Console-win10-x64

rm -r "%BUILD_DIR%"
dotnet publish Demeton.Console -c Release -r win10-x64 ^
	--output "%cd%\%BUILD_DIR%"
