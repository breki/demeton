- fix `Supports parsing elevation coloring step with custom color scale` test case
- log shade pipeline actions

## Tue 05.11.
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
- Implemented basic projection and inverse functions for Web Mercator projection.

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