module Demeton.WorldCover.Fetch

open System
open System.IO
open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Demeton.Geometry.Common
open Demeton.WorldCover.Types
open FileSys
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open BitMiracle.LibTiff.Classic
open Raster

/// <summary>
/// Construct the file name for the cached WorldCover geoJSON file.
/// </summary>
let worldCoverGeoJsonCachedFileName cacheDir =
    Path.Combine(
        cacheDir,
        WorldCoverDirName,
        "esa_worldcover_2020_grid.geojson"
    )


let ensureGeoJsonFile
    cacheDir
    fileExists
    (downloadFile: string -> string -> string)
    =
    let cachedFileName = worldCoverGeoJsonCachedFileName cacheDir

    if fileExists cachedFileName then
        cachedFileName
    else
        downloadFile geoJsonUrl cachedFileName


/// <summary>
/// List all available WorldCover files by reading the specified WorldCover
/// geoJSON file.
/// </summary>
let listAllAvailableFiles
    (openFileToRead: FileReader)
    (geoJsonFile: string)
    : DemTileId seq =
    match openFileToRead geoJsonFile with
    | Ok stream ->
        use reader = new StreamReader(stream)
        use jsonReader = new JsonTextReader(reader)
        let serializer = JsonSerializer()
        let jsonData = serializer.Deserialize<JObject>(jsonReader)

        let features = jsonData["features"] :?> JArray

        features
        |> Seq.map (fun feature ->
            let properties = (feature :?> JObject)["properties"] :?> JObject
            properties["ll_tile"].Value<string>() |> parseTileName)

    | Error e -> failwith e.Exception.Message

/// <summary>
/// Given a bounding box, returns a sequence of WorldCover files that cover it.
/// Each file covers a 3x3 degree area.
/// </summary>
let boundsToWorldCoverFiles (bounds: LonLatBounds) : DemTileId seq =
    let minTileX =
        (bounds.MinLon / float WorldCoverDegreesPerFile)
        |> floor
        |> int
        |> (*) WorldCoverDegreesPerFile

    let minTileY =
        (bounds.MinLat / float WorldCoverDegreesPerFile)
        |> floor
        |> int
        |> (*) WorldCoverDegreesPerFile

    let maxTileX =
        ((bounds.MaxLon / float WorldCoverDegreesPerFile) |> ceil |> int) - 1
        |> (*) WorldCoverDegreesPerFile

    let maxTileY =
        ((bounds.MaxLat / float WorldCoverDegreesPerFile) |> ceil |> int) - 1
        |> (*) WorldCoverDegreesPerFile

    seq {
        for tileY in [ minTileY..WorldCoverDegreesPerFile..maxTileY ] do
            for tileX in [ minTileX..WorldCoverDegreesPerFile..maxTileX ] do
                yield demTileXYId tileX tileY
    }

/// <summary>
/// Returns the download URL for the given WorldCover tile.
/// </summary>
let worldCoverTileDownloadUrl (tileId: DemTileId) : string =
    sprintf
        "%s/%s/%i/map/ESA_WorldCover_10m_%i_%s_%s_Map.tif"
        WorldCoverS3Domain
        WorldCoverVersion
        WorldCoverYear
        WorldCoverYear
        WorldCoverVersion
        tileId.FormatLat2Lon3

/// <summary>
/// Returns the path to the cached TIFF file for the given AW3D tile.
/// </summary>
let worldCoverTileCachedTifFileName cacheDir (tileId: DemTileId) =
    Path.Combine(cacheDir, WorldCoverDirName, $"{tileId.FormatLat2Lon3}.tif")


/// <summary>
/// Calculates the tile ID of the WorldCover file that covers the specified
/// one-degree DEM tile.
/// </summary>
let containingWorldCoverFileTileId (singleDegreeTileId: DemTileId) : DemTileId =
    match singleDegreeTileId.Level with
    | Level0 ->
        let worldCoverTileX =
            singleDegreeTileId.TileX / WorldCoverDegreesPerFile
            * WorldCoverDegreesPerFile

        let worldCoverTileY =
            singleDegreeTileId.TileY / WorldCoverDegreesPerFile
            * WorldCoverDegreesPerFile

        demTileXYId worldCoverTileX worldCoverTileY
    | HigherLevel ->
        raise
        <| InvalidOperationException "Higher-level DEM tiles are not supported."


/// <summary>
/// Ensures the specified AW3D tile TIFF file is available in the cache
/// directory, downloading it if necessary.
/// </summary>
let ensureWorldCoverFile cacheDir fileExists downloadFile tileId: FileName =
    let cachedTifFileName = worldCoverTileCachedTifFileName cacheDir tileId

    if fileExists cachedTifFileName then
        cachedTifFileName
    else
        // download the tile
        let tileUrl = tileId |> worldCoverTileDownloadUrl

        Log.debug
            $"Downloading WorldCover tile {tileId.FormatLat2Lon3} from {tileUrl}..."

        downloadFile tileUrl cachedTifFileName



let ensureWorldCoverFiles
    cacheDir
    (bounds: LonLatBounds)
    : (DemTileId * FileName) list =
    Log.info "Ensuring all needed WorldCover files are there..."

    let geoJsonFile = ensureGeoJsonFile cacheDir fileExists downloadFile

    let allAvailableFiles = listAllAvailableFiles openFileToRead geoJsonFile

    let filesNeeded = bounds |> boundsToWorldCoverFiles |> Seq.toList

    let availableFilesNeeded =
        filesNeeded
        |> List.filter (fun tileId ->
            allAvailableFiles
            |> Seq.exists (fun availableTileId -> availableTileId = tileId))

    availableFilesNeeded
    |> List.map (fun tileId ->
        tileId,
        ensureWorldCoverFile cacheDir fileExists downloadFile tileId)

let copyTiffTileToWorldCoverRaster
    (worldCoverData: DemHeight[])
    (tiffTileBuffer: byte[])
    tiffTileWidth
    tileMinX
    tiffTileY
    : unit =
    let tiffTileBufferSize = tiffTileBuffer.Length

    // local coordinates of the pixel within the TIFF tile
    let mutable tiffTileLocalX = 0
    let mutable tiffTileLocalY = 0
    // coordinates of the pixel within the heights array
    let mutable heightsArrayX = tileMinX
    // we flip the Y coordinate since DEM heights array
    // is flipped vertically compared to the bitmap
    let mutable heightsArrayY = WorldCoverBitmapSize - tiffTileY - 1
    // index of the pixel within the heights array
    let mutable destIndex = heightsArrayY * WorldCoverBitmapSize + heightsArrayX

    for i in 0 .. tiffTileBufferSize - 1 do
        // copy the pixel to the heights array only if it fits within
        // the array (some TIFF tiles may be partially outside the
        // requested area)
        if
            heightsArrayX >= 0
            && heightsArrayY >= 0
            && heightsArrayX < WorldCoverBitmapSize
            && heightsArrayY < WorldCoverBitmapSize
        then
            let value = tiffTileBuffer.[i]
            worldCoverData.[destIndex] <- int16 value

        if tiffTileLocalX + 1 < tiffTileWidth then
            tiffTileLocalX <- tiffTileLocalX + 1
            heightsArrayX <- heightsArrayX + 1
        else
            tiffTileLocalX <- 0
            tiffTileLocalY <- tiffTileLocalY + 1
            heightsArrayX <- tileMinX
            heightsArrayY <- heightsArrayY - 1
            destIndex <- heightsArrayY * WorldCoverBitmapSize + heightsArrayX



/// <summary>
/// Reads a WorldCover TIFF file and returns the heights array for it.
/// </summary>
/// <remarks>
/// The function accepts the tile coordinates of the lower left corner
/// of the WorldCover TIFF file. Note that the TIFF file covers a 3x3 degree
/// area.
/// </remarks>
let readWorldCoverTiffFile
    cacheDir
    (cropBounds: Rect option)
    (worldCoverTileId: DemTileId)
    : HeightsArray =
    let worldCoverTiffFileName =
        worldCoverTileCachedTifFileName cacheDir worldCoverTileId

    use tiff = Tiff.Open(worldCoverTiffFileName, "r")

    if tiff = null then
        failwithf $"Could not open the file %s{worldCoverTiffFileName}"

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

    let worldCoverData: DemHeight[] =
        Array.zeroCreate (WorldCoverBitmapSize * WorldCoverBitmapSize)

    // the size of an individual TIFF tile (not geographic tile, but a tile
    // in terms of tiling a TIFF raster into small quadratic pieces)
    let tiffTileWidth = unbox (tiff.GetField(TiffTag.TILEWIDTH).[0].Value)
    let tiffTileHeight = unbox (tiff.GetField(TiffTag.TILELENGTH).[0].Value)

    // the memory size (in bytes) of a single TIFF tile
    let tiffTileBufferSize = tiff.TileSize()
    let tiffTileBuffer = Array.zeroCreate<byte> tiffTileBufferSize

    let cellMinX, cellMinY =
        tileMinCell
            WorldCoverTileSize
            { Level = { Value = 0 }
              TileX = worldCoverTileId.TileX
              TileY = worldCoverTileId.TileY }

    // for each TIFF tile
    for tiffTileY in [ 0..tiffTileHeight..WorldCoverBitmapSize ] do
        for tiffTileX in [ 0..tiffTileWidth..WorldCoverBitmapSize ] do
            // determine whether the tile is within the crop bounds
            // and skip it if it is not

            let loadTile =
                if cropBounds.IsSome then
                    // flip around Y-axis because TIFF Y-axis is opposite
                    // from DEM one's
                    let tiffTileDemMaxY = (WorldCoverBitmapSize - 1) - tiffTileY
                    let tiffTileDemMinY = tiffTileDemMaxY - tiffTileHeight + 1

                    // calculate TIFF tile's geometric bounds
                    let tiffTileBounds =
                        { MinX = tiffTileX + cellMinX
                          MinY = tiffTileDemMinY + cellMinY
                          Width = tiffTileWidth
                          Height = tiffTileHeight }

                    doRectsIntersect tiffTileBounds cropBounds.Value
                else
                    true

            if loadTile then
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
                // containing the specified coordinates).

                // The tile does not start at tiffTileX, tiffTileY, so we need to
                // calculate its actual starting coordinates and adjust
                // that in our calculations when copying to the worldCoverData array.

                // Log.debug $"Reading TIFF tile {tiffTileX}/{tiffTileY}..."

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

                // X coordinate (column) of this TIFF tile in the grid of TIFF tiles
                let tileGridColumn = tiffTileX / tiffTileWidth

                // minimum/corner X cell coordinate of the TIFF tile in the WorldCover
                // grid of cells
                let tileMinX = tileGridColumn * tiffTileWidth

                // Copy the contents of the tile buffer into the worldCoverData
                // array. This way we fill the heights array by the contents of each
                // TIFF tile, one by one, until we have the whole heights array
                // filed.
                // Note that we use int16 heights array here because of simplicity,
                // but in future we should provide direct support for byte arrays
                // (since WorldCover uses byte arrays).

                // todo sometime 100: not exactly optimal way to copy from one array to another
                copyTiffTileToWorldCoverRaster
                    worldCoverData
                    tiffTileBuffer
                    tiffTileWidth
                    tileMinX
                    tiffTileY
            else
                ()

    let raster =
        HeightsArray(
            cellMinX,
            cellMinY,
            WorldCoverBitmapSize,
            WorldCoverBitmapSize,
            HeightsArrayDirectImport worldCoverData
        )

    if cropBounds.IsSome then
        let x = raster |> extract cropBounds.Value
        let analysis = analyzeHeightsArray x
        x
    else
        raster
