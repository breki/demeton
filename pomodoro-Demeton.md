## Wed 25.9.
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