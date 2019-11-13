SET BUILD_DIR=builds\Demeton.Console
SET CACHE_DIR=samples\cache
SET CONSOLE="%BUILD_DIR%\Demeton.Console.exe"

%CONSOLE% import 13,45,17,49 --local-cache-dir %CACHE_DIR% ^
	--srtm-dir \\hobbit\SRTM
