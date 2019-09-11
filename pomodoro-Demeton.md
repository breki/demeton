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