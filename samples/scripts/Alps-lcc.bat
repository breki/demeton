REM Generates a hillshaded, elevation colored map of the Alps, 
REM in Lambert Conformal Conic projection 

builds\Demeton.Console\Demeton.Console.exe ^
	shade 4.285186,42.795401,17.203536,48.618385 ^
	--proj "+proj=lcc +lon_0=10.744361 +lat_0=45.706893 +lat_1=40 +lat_2=50" ^
	--map-scale 2500000 --dpi 72 --tile-size 5000 ^
	--shading-script ^
	"elecolor(scale=0:#ccf3ff;1:#8ed48e;700:#f5fac4;1500:#d9d7bd;2500:#f2ebd2;3500:#ffffff;none:#ccf3ff)|+igor" ^
	--local-cache-dir G:\SRTM-PNG --srtm-dir G:\SRTM


