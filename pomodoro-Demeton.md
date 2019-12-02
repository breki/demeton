- dsl for isoline building?
- refactor the code to avoid using height and isoline values directly and instead provide a comparison function


## Mon 02.12.
- Separated isoline H and V coordinate types, directions and steps.
- Separated horizontal from vertical isoline directions.
- Added test property for asserting that iso point is present in one and only one isoline.
- Added test property for checking that the closed isoline is really closed.
- Added test property for checking whether the isoline steps follow previous ones.
- Separated isoline production code from unit and property tests.
- Finally (hopefully) fixed the isoline detection algorithm to pass all property tests. I still have to clean the code and refactor it to stop using height values directly.

## Sun 01.12.
- Switched to using integers in isoline properties test.
- Fixing various bugs in isoline detection algorithm.

## Sat 30.11.
- Added the ability to search for multiple isolines of the same value.
- Implemented isolines tracing.
- Implemented the first simple case for isoline tracing.
- Added `LICENSE-3RD-PARTY.txt` document.
- Moved common projection functions into `Common` module.

## Fri 29.11.
- Various bug fixes and implementations of missing error handling.
- Introducing map projection validation step before the creation.
- Integrated map scale into LCC code.
- Finished working on LCC, for now.
- Fixed a bug in `phi2z` function, so LCC now calculates correct inverse values.
- PROJ: added support for parsing parameters without values (like `+no_defs`).

## Thu 28.11.
- Started working on the Lambert Conformal Conic projection support.
- Restructured the PROJ code, renamed some modules and types.

## Wed 27.11.
- Implemented all scenarios for `parseProjSpecParameter`. 
- Fixed a bug in PROJ parsing.
- Replaced the old Mercator projection functions with the new `Mercator.MapProjection` class.
- `MapScale` and `ProjectionScaleFactor` are now embedded in the map projection, so they don't need to be carried around separately.
- Introduced `MercatorMapProjection` class to begin merging map scale with projection.
- Extracted `MapProjection` record holding the two projection functions. This is now used in `ShadeCommand.Options`.

## Tue 26.11.
- Updated the Roadmap with map projection developments.
- Finished writing the tutorial, for now.
- Defined a map projection factory function.
- Started adding command line support for projections.
- Beautified `build.bat` output a little.
- Moved the projection parsing code to the production module.

## Mon 25.11.
- Introduced indirection when using map projection, the production code no longer directly calls `Mercator`'s functions.
- Implemented rudimentary PROJ parsing to Mercator projection.
- Continued working on the tutorial.
- Introduced `Level0` and `HigherLevel` active patterns.
- Fixed: if shade command has no tiles to work on, it does nothing, but it doesn't tell it to the user.
- Implemented the basic PROJ parsing into a list of parameters.

## Sun 24.11.
- Renamed incorrectly named `WebMercator` to `Mercator`.
- The igor hillshading used in the default shading script had -90 degrees instead of -45.
- Started writing the tutorial.
- Included the `download-sample-data.sh` script in the release.
- Fixing the returning result of `ShadeCommand.run`.
- The console now prints out the GitHub repo link.
- `README.md` is now included in the distribution.
- Added `download-sample-data.sh` script.

## Sat 23.11.
- Added a description of release packages.
- Added sample Alps images.
- Included the license file in the distribution.
- Written the `Roadmap.md` document.

## Fri 22.11.
- Added a simple smoke test to `release.sh`.
- `release.sh` now accepts a release version number.
- `build.sh` is now running on my local Ubuntu.
- Fixed a couple of tests to no longer use `\` for paths.
- Added skipping of loading 16-bit grayscale PNGs using GDI+ in tests since it doesn't work on vanilla Ubuntu 18.04
- Prepared WLS Ubuntu machine so I can test build scripts on Linux.

## Thu 21.11.
- Turned off localized satellite assemblies generation (using `SatelliteResourceLanguages` project property).
- Implemented the release procedure (mostly).
- Exposed `cellsPerDegree` as a parameter for Tile coord functions to speed things up.
- Inlined some of short Tile functions.
- Implemented functional error handling of opening streams, but it has its own problems (https://www.reddit.com/r/fsharp/comments/dzm42d/disposables_and_results/).
- Started introducing functional error handling in `FileSys`.
- `shade` command now writes the size of the total raster and tiles needed before starting generating them.

## Wed 20.11.
- Improved logging if tile fetching code.
- Fixed the wrong order of compositing images (source vs. destination).
- Extract helper function(s) for sample files (GetManifestResourceStream).
- (Re)defined `HeightsArrayResult` and defined `HeightsArrayMaybeResult` types.
- Defined `SrtmTile` as type for SrtmTileId * HeightsArray.
- Refactored the tile fetching code to keep the heights arrays in memory instead of having to re-read them.
- Implemented huge speed-up of HGT tiles reading by first copying the zipped stream into a MemoryStream.
- Extracted `openZippedHgtFileStream` so it can be reused in tests.
- Prepared a sample batch script for generating hillshading of Alps.

## Tue 19.11.
- Fixing another issue in tile processing.
- Bugfix: `writeSrtmTileToLocalCache` does not ensure the directory exists.
- Added the investigation notes and links to the `Downsampling` module.
- Redesigned downsampleAverage to use 4 parameters instead of an array.
- Switched to using `Average` downsampling.
- Implemented the `downsampleAverage` function.
- Moved `readPngTilesBatch` tests into its own file.
- Introduced `DownsamplingMethod` and moved all of the downsampling code into a separate module.

## Mon 18.11.
- Integration test is not run if the SRTM_DIR env. var. is missing.
- Added `build.sh` build script.

## Sun 17.11.
- `merge` function has now been redesigned to be much faster.
- Redesigning the `HeightsArray` `merge` function and writing property tests for it.
- Fixing various bugs/issues with tile coordinate system.

## Sat 16.11.
- Remove obsolete tile cache status checking code.
- Renaming and moving some old `Tile` functions.
- Started introducing the new tiles coordinate system into the code.
- Moved the new tile coordinate code to production assembly.
- Implemented generating and parsing tile names.
- Started working on a new tile coordinate system.

## Fri 15.11.
- Extended `decodeSrtmTileFromPngFile` to support levels.
- A level 1 tile is finally generated, yay!
- Implemented `writeSrtmTileToLocalCache` function.
- Extracted the higher tile-creating logic from  `processNextCommand` into a separate method to be more testable.
- A lot of wiring and refactoring.

## Thu 14.11.
- Finished implementing `finalizeFetchSrtmTileProcessing` function.
- Moved functions from integration tests to the `Fetch` module.
- Decomposed new `fetchSrtmTile` function.
- `processCommandStack` now handles the `Failure` command indicator.
- "Convert from PNG" function now supports Result.
- Added some integration tests for fetching SRTM tiles.
- Moved the tile fetching logic from the test assembly to the production module.
- Implemented the basics of the first acceptance test that calls the console, but it's not finished since levels support is not done.

## Wed 13.11.
- Added `test-data.bat` script to import tiles for the sample area.
- Fixed: help with unknown command is not finished yet.
- Fixed: the console returns no error output.
- Fixed: the console reports an error when no command is specified.
- Improved build scripts, added steps to publish Demeton console to the `builds` directory.
- Implemented tail-recursive `processCommandStack` command.
- Finished implementing the first version of `processNextCommand` function for fetching SRTM tiles.

## Tue 12.11.
- Trying out a new, more functional approach on checking the tile status.
- Finished implementing the new functions for checking the caching status of the tile.

## Mon 11.11.
- Removed `SrtmTileFile` record because it was just a design obstacle.
- Started working on a new implementaton ofr `fetchSrtmTile` code because the old one was just too unwieldly to support levels.  

## Sun 10.11.
- Started working on fetching SRTM tiles from higher levels.
- `ShadeCommand.generateShadedRasterTile` now works with SRTM levels.
- `ShadeCommand.run` now calculates the SRTM level needed and feeds it to the tile generator.
- `generateSampleWithParameters` now takes into account the SRTM level and generates an appropriate heights array using it.
- Fixed tests that use ShadingSampleGenerator now that tile level is taken into account.
- `RasterShader` now has the SRTM level as a parameter.
- `longitudeToGlobalX` and `latitudeToGlobalY` now take SRTM level into account.
- Level needed was wrongly calculated, it did not convert the delta into SRTM cell delta first.
- Renamed `MapProjectionParameters` and `ShaderOptions` to `MapScale`.
- Implemented a method that, for a given min lon/lat delta, calculates the DEM level needed.
- Introduced helper srtmTileCoords function for tests.
- Introduced `SrtmLevel`.

## Sat 09.11.
- Tile cache is now organized into level subfolders.
- `boundsToTiles` function now works with tile levels.
- `SrtmTileCoords` now has `Level` property.
- Moved the "min lon/lat delta" to a projection module and documented it.
- Renamed `ShaderOptions` should be to `MapProjectionParameters` and moved to projections module.
- Implemented the first version of "min lon/lat delta" algorithm.
- Finished implementing simulated annealing algorithm.

## Fri 08.11.
- Started working on a simulated annealing algorithm.
- Working on implementing the calculation for the needed SRTM level based on the map scale and DPI.

## Thu 07.11.
- Extended tests for `executeShadingStep` for all supported steps.
- Switching to using constants for shade step names.
- Implemented `aspectShaderStepBuilder`.

## Wed 06.11.
- Implemented `slopeShaderStepBuilder`.
- Implemented `igorHillshadingStepBuilder`.
- Implemented a reusable way to fetch errors from fparsec parsing.
- Moved `CommandLine.TextParsers` to its own top-level module.

## Tue 05.11.
- Added more logging to the operations run by `shade` command.
- Fixed `Supports parsing elevation coloring step with custom color scale` test case.
- Added more property tests for `IgorHillshader`
- Fixing wrong calculation of aspect in `Hillshading`, added some tests for it.
- `IgorHillshader.ShaderParameters` now has a single color property, `ShadingColor`.
- Added sea color to the default color scale.
- Parallelized `ElevationColoring.shadeRaster`.
- SRTM cells are now treated as having coordinates on the geometric centers, so 0.5 offset is no longer used.

## Mon 04.11.
- Fixed a bug in the SRTM tiles coordinate system.
- Added the new shading script parsing into the console code.
- `shade` command now prints out the shading script error location.
- Added support for multiline command line parsing errors.
- `buildShadingPipeline` function did not handle the case when the step was not recognized.

## Sun 03.11.
- Implemented handling of errors when shading step building in `buildShadingPipeline` function.

## Sat 02.11.
- Implemented a generic way of building and parsing step parameters, so we can return errors in an unified way.

## Wed 30.10.
- Implement the positive scenario for parsing elevation color scale parameter in the shading script.

## Tue 29.10.
- Started working on parsing specific shading steps.
- `buildShadingPipeline` now accepts a dictionary of registered step builders.
- `ShadingStep.CustomShading` no longer has a direct `RasterShader` function as a property, it now has a string identifier of the shader function, which is then used by `ShadingFuncFactory` to access the actual function.
- `ShadingStep.Compositing` no longer has a direct compositing function as a property. Instead, it now has a string identifier of the function, which is then used by `CompositingFuncFactory` to access the actual function. This makes things easier to work with and test.
- Move the new parsing code to the production module.
- Implemented negative parsing scenarios for the tokenizing parser.
- Implemented the tokenizing parser for shading scripts.

## Mon 28.10.
- Moved elevation coloring parser to the production module.
- Extracted color generators into `ColorGen` module.
- Implemented `colorScaleToString` function which is now used in tests.

## Sun 27.10.
- Started working on the elevation color scale parser.
- Implemented parser for color hex triplets and quadruplets.
- Studying and experimenting with FParsec. Added some exploratory tests for it.

## Sat 26.10.
- Started working on the shading script parser.
- Switched to using concrete steps in `Demeton.Shaders.ShadingPipeline.ShadingStep`.
- Speeded up `Png.AlphaCompositing.imageOver` function using parallelization and inlining.
- Fixed a bug in alpha compositing code, it seems to be working now.
- Moved shading pipeline code to the production module (`Demeton.Shaders.ShadingPipeline`).
- Implemented `alphaCompositingOver` function.

## Fri 25.10.
- Implemented a prototype shader pipeline framework.

## Thu 24.10.
- Written property tests for alpha compositing.

## Wed 23.10.
- `Hillshading.shadeRaster` function now parallelizes the shading of the raster, line by line.
- Extracted slope and aspect calculation into a separate function.
- `ShadingIntensity` is no longer available in `ShaderParameters`.
- `ShaderParameters` record is now specific to `IgorHillshader`.
- Moved elevation coloring shader to its module.
- Made the pixel shader names consistent.
- Moved code from `NewHillshader` module to `Hillshader`.
- `Demeton.Shaders.Hillshading` module now requires qualified access.
- Removed the "old" (2x2) slope and aspect calculation and shading function.
- Implemented the aspect + slope calculation based on Horn's formulas.

## Tue 22.10.
- Reorganizing the shader code a bit, so I can implement a new hillshader based on some scientific articles.

## Mon 21.10.
- Implemented SlopeShader and AspectShader and discovered something is wrong with the aspect calculation.

## Sun 20.10.
- Finished working on CLI framework (for now).
- Extended `validateSupportedParameters` function to checks that all non-mandatory command arguments are after mandatory ones.
- Implemented support for non-mandatory command arguments.
- Added `Switch` module for building switches.
- Added `Option` module for building options.
- Added `Arg` module for building arguments.
- Command line usage now indicates when an argument is optional.
- Added `IsMandatory` property to `CommandArg`.
- Implemented `HelpCommand.commandDescription` function.

## Sat 19.10.
- Finished implementing rendering of detailed help for command usage and parameters.

## Fri 18.10.
- Unified the default parameter values into constants.
- Implemented rendering of command arguments, options and switches.
- Fixed a bug in one of `Mixing Rgba8Bit colors` properties.

## Thu 17.10.
- Started working on rendering command parameters help.
- Implemented displaying of available commands in `help` command.
- `import` and `shade` commands are now using the new system for parsing whole command lines.
- Finished implementing parsing of whole command lines.

## Wed 16.10.
- Fixed a bug in FsCheck generators in `PropertiesHelp`.
- `import` and `shade` commands now use positional command arguments for mandatory parameters.
- Implemented support for positional command line arguments.
- Defined a `CommandArg` and related.

## Tue 15.10.
- Made a complete redesign of the command line parsing code so we could get rid of generics in the parameters specifications.

## Mon 14.10.
- Implemented a `parseParameters` function for parsing commands in a more generic way.

## Sun 13.10.
- Extracted common `CommandLineParsingFunction` and `CommandLineParameter` types and implemented a more generic parsing code for the existing commands.
- Added `--elev-color` switch to the `shade` command.
- `generateShadedRasterTile` function now accepts `RasterShaderFactory` instead of `RasterShader` directly.
- Introduced `Shader` into `ShadeCommand.Options`.
- Projection code now uses radians instead of degrees.
- PixelHillshader now returns Rgba8Bit.RgbaColor.
- Started working on extending the `shade` command to support pixel hillshaders.

## Sat 12.10.
- Started writing tests for Igor's hillshading method.
- Renamed "orientation" term to the more appropriate "aspect".
- Reimplemented production calculation of the orientation.
- Finally (hopefully) fixed all the issues with the reference orientation calculation.

## Thu 10.10.
- Implemented orientation calculation for the terrain.
- Implemented a new and better `slope` function.

## Wed 9.10.
- Working on a better `slope` function.

## Tue 8.10.
- Implemented `slope` function.
- Written tests for these common functions.
- Moved some common geometry functions from `Demeton.Hillshading` module to `Demeton.Geometry.Common`.

## Mon 7.10.
- Fixed a bug in color mixing, so elevation coloring is working now.
- Added property tests for elevation coloring.
- Added a property test for mixing 8-bit RGBA colors.
- Added a helper `PropertiesHelper` module for testing with FsCheck.
- Learning more about FsCheck.

## Sun 6.10.
- Added elevation color scale from Maperitive, but it renders it oddly.
- shade command now has the `--tile-size` command line parameter.
- Removed support for optional color for `DemHeightNone` in `ElevationColorScale`.

## Sat 5.10.
- Implemented 8-bit RGBA color mixing.
- Started implementing `ElevationColorScale` functions.

## Fri 4.10.
- Cleaned up `Saving the tile.fs`
- Prepared the `shade` command so it runs now.
- Fixed the wrong return type in `generateShadedRasterTile` function.
- Extracted the console wiring code into its own `Wiring` module.
- Finished implementing the `saveShadedRasterTile` function.
- Split `ShadeCommand` tests into multiple files.

## Thu 3.10.
- Implemented a prototype `shadeRaster` function.
- More test cases for `generateShadedRasterTile` function.

## Wed 2.10.
- Implemented partitioning of the hillshaded raster into tiles.
- Started implementing a common geometry code needed for the shade command.

## Tue 1.10.
- Renamed things in the commands code.
- Added support for all the `shade` command parameters (at least for now).
- Implemented some basic tests for Geometry functions.
- Renamed `Demeton.GeometryTypes` to `Demeton.Geometry`.
- Continued working on `ShadeCommand` parsing.
- ImportSrtmTilesCommand: replaced the existing float list parsing code with the new FParsec one.

## Mon 30.9.
- Introduce FParsec for parsing the command line.
- Started working on the shade command.
- Implemented basic projection and inverse functions for Mercator projection.

## Sun 29.9.
- Corrected `longitudeToGlobalX` and `longitudeToGlobalY` functions to take into account that the rounded (non-fractional) coordinates represent each cell's center point, not it bottom left one.

## Sat 28.9.
- Implemented `Demeton.Srtm.Tile.longitudeToGlobalX` and `Demeton.Srtm.Tile.latitudeToGlobalY` functions.
- Implemented support for 8-bit RGBA PNG images.
- Renamed `Png.PixelFormats` module into `Png.Grayscale16Bit` since we will also have a separate module for RGBA images.
- Reordered and reorganized test files.

## Fri 27.9.
- `HeightsArray` now supports `HeightsArrayCustomInitializer`, which is used by `decodeSrtmTileFromPngFile`.`generateHeightsArray` function to speed things up.
- `HeightsArray` now supports `HeightsArrayDirectImport` way of constructing it, which is now used in `createSrtmTileFromStream` function.
- Added logging to `convertZippedHgtTileToPng` function.
- `readSrtmHeightsFromStream` now returns array directly.
- `readSrtmHeightsFromStream` now skips the STRM tile's additional row and column of heights (since we don't need it and makes working with the resulting array much easier).
- Fixed a couple of bugs in the code that went unnoticed.
- Added a license file.
- Removed `ensureTilesAreInCache` function since it's not really being used in the production code.
- Introduced `HeightsArrayInitializer` and `Grayscale16BitImageDataInitializer` so we can provide multiple ways to initialize these arrays.
- Switched to using 1D array instead of 2D for HeightsArray (for performance reasons).
- Stopped using an optional height in the HeightsArray (for performance reasons).

## Thu 26.9.
- Migrated to .NET Core 3.0.
- `decodeSrtmTileFromPngFile` now returns Result instead of throwing exceptions.
- Implemented `ResultSeq`.
- `SrtmTileReader` now returns a Result to improve error handling.

## Wed 25.9.
- Implemented a simple logging API.
- Renamed `ImageData` to `RawImageData`.
- Introduced `CachingStatus` to avoid the import command having to read already imported tiles.
- Removed some obsolete code from the console project.
- Separated SRTM tile-oriented functions into `Tile` module.
- Separated SRTM code into its own subnamespace and renamed its modules.
- Renamed PNG modules so they start with `Png` namespace.
- Fixed "Imports all tiles within the specified boundaries" test case to run now that the import function employs parallelization.
- Renamed `Demeton.Crc` module to `Crc` and added `RequireQualifiedAccess` attribute to it.
- Renamed `Demeton.Binary` module to `Bnry` and added `RequireQualifiedAccess` attribute to it.
- Renamed `Paths` module to `Pth`.
- Renamed `FileSystem` module to `FileSys` and added `RequireQualifiedAccess` attribute to it.

## Tue 24.9.
- Pieced together all of the code for the import tiles command.
- Added FAKE build script.
- Added more code documentation.

## Mon 23.9.
- Implemented `encodeHeightsArrayIntoPngFile` function.
- Implemented `openZipFileEntry` function.
- Finished implementing `convertZippedHgtTileToPng` function.
- Finished implementing `fetchSrtmTile` function.
- Separated the generic command line parsing code and its tests.
- Made a more generic way of parsing command line parameters.
- Made a more generic way of constructing error messages in the command line parser.
- More test cases for  import command line parsing.

## Sat 21.9.
- Implemented more test cases for import command line parsing.

## Thu 19.9.
- Started working on the console command line interface.
- Implemented the first test case in ImportSrtmTilesCommand tests.
- Tried out various deflate parameters to improve the compression rate, but none of them did much.
- Finished optimizing PNG decoding.
- Fixed a bug in `Demeton.Binary.readBytes()` that prevented the method from ever finishing.
- Added some test commands into the `Demeton.Console` so can test the real-life speed of PNG encoding/decoding.
- Speeded up `grayscale16BitImageData` function.
- Reorganized and renamed PNG modules.

## Wed 18.9.
- Implemented an alternative PNG filtering code which runs around 20% faster.
- Chosen the fastest `crc32` variant as the new implementation.
- Implemented several alternative `crc32` functions and added benchmarks for them.
- Updated the "deserializing/serializing IDAT chunk" test case.

## Tue 17.9.
- No longer using a 2D array to represent 8-bit and 16-bit grayscale images.
- `IhdrData` now has the `BitsPerPixel` calculated property.
- `loadPngFromStream` function now returns an `IhdrData` * `ImageData` tuple.

## Mon 16.9.
- Added 2D vs 1D array access benchmarks.
- `filterScanlines` now, instead of creating an array for each filtered scanline, uses one single `FilteredImageData` and feeds the scanline data into it.
- Redesigned `ScanlinesGenerator` to not use obsolete `Scanline` type abbreviation.
- `filterScanlines` function now returns `FilteredImageData` instead of an array of `FilteredScanlines`.
- Images no longer work with scanlines, but with raw image data byte arrays instead.
- Switched to using Spans for unfiltering PNGs.

## Sun 15.9.
- Added benchmarks for byte indexer vs Span access.
- Speeded up `unfilterScanlineNone` by using `Array.blit` instead of array comprehension.
- Added a simple build script that runs tests.
- Added a SRTM tile rendered as PNG as a sample so we can test decoding of PNGs.
- Added one more sample SRTM tile. 
- Removed duplicate sample SRTM tile from the tests project.
- For performance reasons, `GlobalCellCoords` now just a type alias for a tuple and no longer a record.

## Sat 14.9.
- Removed old filtering code.
- Introduced parallelism into scanlines filtering.
- Optimized the `scanlineFilterMultiple` function so PNG encoding is much faster now.

## Thu 12.9.
- Refactored the filter code to calculate a single value instead of the whole scaline, to remove code duplication.
- Refactored the filter code to calculate the sum of absolute differences during the filtering.
- Since the whole thing is very slow, I will no work on optimizing the code.
- Working on generating PNGs from SRTM HGTs.

## Wed 11.9.
- Implemented `loadPngFromStream` function.
- Implemented `saveGrayscale16BitToStream` function.
- Implemented `saveGrayscale8BitToStream` function.
- Implemented bpp support for "Paeth" filter type and thus finished with introducing bpp into filtering.
- Implemented bpp support for "Average" filter type.
- Implemented bpp support for "Sub" filter type.
- Introduced bbp (bytes per pixel) parameter into IDAT chunk handling and filtering, but I haven't yet updated the filtering code to use it.

## Tue 10.9.
- Extracted `Demeton.PngChunks` module.
- Extracted `Demeton.PngPixelFormats` module.
- Moved functions for binary reading and writing into `Demeton.Binary` module. 
- Moved PNG code into Png folder.
- Finished implementing encoding/decoding of 8-bit grayscale PNG images.
- Continued working on PNG decoding for the simplest case.
- Implemented `deserializeIhdrChunkData` function and added property test for it.
- Moved `compress` and `decompress` functions to `Demeton.Png` module.
- Started working on PNG compression.

## Mon 9.9.
- Flattened `PNG filtering.fs` tests by using custom property attribute.
- Moved PNG functions into Demeton modules.
- Documented `PngTypes.fs`.
- Moved PNG type definitions to `PngTypes.fs`.
- Extracted filter selection as an extra filter function.
- Implemented unfilterScanlines function.

## Sun 8.9.
- Implemented filterScanlines function.
- Implemented filterScanlinePaeth function.
- Implemented filterScanlineAverage function.
- Implemented some basic property testing for PNG filters.

## Sat 7.9.
- Introduced FsCheck to test filtering.
- PNG tests now generate image with random pixels.
- Implemented `filterScanlineSub` function.
- Implemented `filterScanlineNone` function.
- Implemented `grayscale8BitScanlines` function.
- Implemented `serializeIendChunkData` function.
- Renamed IhdrChunk to IhdrData.
- Refactored the PNG chunk writing code to support writing of various chunk types.

## Fri 6.9.
- Implemented "Writes chunk into a stream" test case.
- Introduced ChunkType.
- Started working on PNG writing.
- Implemented an integration test reading a HGT file.

## Thu 5.9.
- Finished implementing `Srtm.createSrtmTileFromStream`.
- Fixed the initialization of cells in `HeightsArray` so it is done only once.
- Inlined some of the methods to improve performance, but it's still way too slow.
- Renamed `HeightArray` to `HeightsArray`.
- Started implementing reading of a whole HGT file.
- Implemented `Srtm.tileCellMinCoords` function.

## Wed 4.9.
- Introduced SrtmLongitude and SrtmLatitude types.

## Tue 3.9.
- Implemented readSrtmHeightsFromStream function.

## Mon 02.09.
- Introduced GlobalCellCoords which helped me root out the bug with HeightsArray merging.

## Sun 01.09.
- the constructor of the HeightArray now accepts a function for initializing
- made some changes in how the HeightArray is structured
- renamed DemData to HeightArray

## Sun 18.08.
- started using single-case unions for some of the types