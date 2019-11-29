REM Generates a hillshaded, elevation colored map of the Alps, 
REM in Lambert Conformal Conic projection 

builds\Demeton.Console\Demeton.Console.exe ^
	shade -15.905549,33.526578,44.470002,60.716198 ^
	--proj "+proj=lcc +lon_0=14.2822265 +lat_0=47.121388 +lat_1=30 +lat_2=65" ^
	--map-scale 25000000 --dpi 72 --tile-size 5000 ^
	--shading-script ^
	"elecolor(scale=0:#ccf3ff;1:#8ed48e;700:#f5fac4;1500:#d9d7bd;2500:#f2ebd2;3500:#ffffff;none:#ccf3ff)|+igor" ^
	--local-cache-dir G:\SRTM-PNG --srtm-dir G:\SRTM


