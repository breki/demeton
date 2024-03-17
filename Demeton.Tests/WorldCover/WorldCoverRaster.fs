module WorldCoverRaster

open Demeton.DemTypes
open Demeton.Srtm.Types
open BitMiracle.LibTiff.Classic

[<Literal>]
let WorldCoverTileSize = 12000

[<Literal>]
let WorldCoverBitmapSize = WorldCoverTileSize * 3


/// <summary>
/// Reads a WorldCover raster file and returns the heights array for a specified
/// tile.
/// </summary>
/// <remarks>
/// The function accepts the SRTM-like tile coordinates of the lower left corner
/// of the WorldCover raster file and the SRTM-like tile coordinates of the tile
/// whose heights array is to be read.
/// </remarks>
let readWorldCoverRaster
    (fileName: string)
    (raster_file_tile_coords: SrtmTileCoords)
    (requested_tile_coords: SrtmTileCoords)
    : DemHeight[] =
    use tiff = Tiff.Open(fileName, "r")

    if tiff = null then
        failwithf $"Could not open the file %s{fileName}"

    let rasterWidth = unbox (tiff.GetField(TiffTag.IMAGEWIDTH).[0].Value)

    if rasterWidth <> WorldCoverBitmapSize then
        failwithf
            $"Expected width %d{WorldCoverBitmapSize}, but got %d{rasterWidth}"

    let rasterHeight = unbox (tiff.GetField(TiffTag.IMAGELENGTH).[0].Value)

    if rasterHeight <> WorldCoverBitmapSize then
        failwithf
            $"Expected height %d{WorldCoverBitmapSize}, but got %d{rasterHeight}"

    let planarConfig: PlanarConfig =
        tiff.GetField(TiffTag.PLANARCONFIG).[0].Value :?> PlanarConfig

    if planarConfig <> PlanarConfig.CONTIG then
        failwithf
            $"Expected CONTIG planar configuration, but got %A{planarConfig}"

    if not (tiff.IsTiled()) then
        failwithf "Expected a tiled TIFF file"

    let unreducedHeightsArrayWidth = WorldCoverTileSize
    let unreducedHeightsArrayHeight = WorldCoverTileSize

    let worldCoverData: DemHeight[] =
        Array.zeroCreate (
            unreducedHeightsArrayWidth * unreducedHeightsArrayHeight
        )

    // calculate the box (in TIFF coordinates) of the requested SRTM tile
    let startingTiffX =
        (requested_tile_coords.Lon.Value - raster_file_tile_coords.Lon.Value)
        * WorldCoverTileSize

    let startingTiffY =
        -(requested_tile_coords.Lat.Value - raster_file_tile_coords.Lat.Value)
        * WorldCoverTileSize

    let endingTiffX = startingTiffX + WorldCoverTileSize
    let endingTiffY = startingTiffY + WorldCoverTileSize

    // the size of an individual TIFF tile (not geographic tile, but a tile
    // in terms of tiling a TIFF raster into small quadratic pieces)
    let tiffTileWidth = unbox (tiff.GetField(TiffTag.TILEWIDTH).[0].Value)
    let tiffTileHeight = unbox (tiff.GetField(TiffTag.TILELENGTH).[0].Value)

    // the memory size (in bytes) of a single TIFF tile
    let tiffTileBufferSize = tiff.TileSize()
    let tiffTileBuffer = Array.zeroCreate<byte> tiffTileBufferSize

    // for each TIFF tile that intersects the requested SRTM tile
    for tiffTileY in [ startingTiffY..tiffTileHeight..endingTiffY ] do
        for tiffTileX in [ startingTiffX..tiffTileWidth..endingTiffX ] do
            /// 0-based byte offset in buffer at which to begin storing read
            /// and decoded bytes
            let offset = 0
            /// z-coordinate of the pixel within a tile to be read and decoded
            let tileZ = 0
            /// The zero-based index of the sample plane. The plane parameter is
            /// used only if data are organized in separate planes
            /// (PLANARCONFIG = SEPARATE). In other cases the value is ignored.
            let plane = int16 0

            // The tile to read and decode is selected by the (x, y, z, plane)
            // coordinates (i.e. ReadTile returns the data for the tile
            // containing the specified coordinates.

            // The tile does not start at tiffTileX, tiffTileY, so we need to
            // calculate its actual starting coordinates and adjust
            // that in our calculations when copying to the unreducedHeightsArray

            let tileXIndex = tiffTileX / tiffTileWidth
            let tileYIndex = tiffTileY / tiffTileHeight

            let actualTileX = tileXIndex * tiffTileWidth
            let actualTileY = tileYIndex * tiffTileHeight

            let bytesInTile =
                tiff.ReadTile(
                    tiffTileBuffer,
                    offset,
                    tiffTileX,
                    tiffTileY,
                    tileZ,
                    plane
                )

            if bytesInTile = -1 then
                failwith "Could not read tile."

            // Copy the contents of the tile buffer into the unreduced heights
            // array. This way we fill the heights array by the contents of each
            // TIFF tile, one by one, until we have the whole heights array
            // filed.
            // Note that we use int16 heights array here because of simplicity,
            // but in future we should provide direct support for byte arrays
            // (since WorldCover uses byte arrays).

            // todo 50: not exactly optimal way to copy from one array to another
            for i in 0 .. tiffTileBufferSize - 1 do
                let value = tiffTileBuffer.[i]

                // local coordinates of the pixel within the TIFF tile
                let tiffTileLocalX = i % tiffTileWidth
                let tiffTileLocalY = i / tiffTileWidth

                // coordinates of the pixel within the heights array
                let heightsArrayX =
                    (actualTileX - startingTiffX) + tiffTileLocalX

                let heightsArrayY =
                    (actualTileY - startingTiffY) + tiffTileLocalY

                // copy the pixel to the heights array only if it fits within
                // the array (some TIFF tiles may be partially outside the
                // requested area)
                if
                    heightsArrayX >= 0
                    && heightsArrayY >= 0
                    && heightsArrayX < unreducedHeightsArrayWidth
                    && heightsArrayY < unreducedHeightsArrayHeight
                then
                    // index of the pixel within the heights array
                    let index =
                        heightsArrayY * unreducedHeightsArrayWidth
                        + heightsArrayX

                    worldCoverData.[index] <- int16 value

    worldCoverData
