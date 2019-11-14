SET CONFIG=Release
SET DEMETON_CONSOLE_DIR="%cd%\builds\Demeton.Console"
SET SRTM_CACHE="%cd%\samples\cache"

dotnet build --configuration %CONFIG% --verbosity minimal --no-incremental
dotnet test --configuration %CONFIG% --verbosity minimal ^
	--filter Category=acceptance
