SET CONFIG=Release
SET BUILD_DIR=builds\Demeton.Console

rm -r "%BUILD_DIR%"

dotnet clean --configuration %CONFIG%
dotnet build --configuration %CONFIG% --verbosity minimal
dotnet test --configuration %CONFIG% --verbosity minimal
dotnet publish Demeton.Console -c %CONFIG% --output "%cd%\%BUILD_DIR%"
