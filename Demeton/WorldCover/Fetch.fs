module Demeton.WorldCover.Fetch

open System.IO
open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Demeton.Geometry.Common
open Demeton.WorldCover.Types
open FileSys
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open BitMiracle.LibTiff.Classic

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
    (openFileToRead: FileWriter)
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

// todo 0: write tests for boundsToWorldCoverFiles or make sure the existing ones work properly
/// <summary>
/// Given a bounding box, returns a sequence of WorldCover files that cover it.
/// Each file covers a 3x3 degree area.
/// </summary>
let boundsToWorldCoverFiles (bounds: LonLatBounds) : DemTileId seq =
    let degreesPerTile = 3

    let minTileX =
        (bounds.MinLon / float degreesPerTile) |> floor |> int |> (*) 3

    let minTileY =
        (bounds.MinLat / float degreesPerTile) |> floor |> int |> (*) 3

    let maxTileX =
        ((bounds.MaxLon / float degreesPerTile) |> ceil |> int) - 1
        |> (*) degreesPerTile

    let maxTileY =
        ((bounds.MaxLat / float degreesPerTile) |> ceil |> int) - 1
        |> (*) degreesPerTile

    seq {
        for tileY in [ minTileY..degreesPerTile..maxTileY ] do
            for tileX in [ minTileX..degreesPerTile..maxTileX ] do
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
/// Ensures the specified AW3D tile TIFF file is available in the cache
/// directory, downloading it if necessary.
/// </summary>
let ensureWorldCoverFile cacheDir fileExists downloadFile tileId =
    let cachedTifFileName = worldCoverTileCachedTifFileName cacheDir tileId

    if fileExists cachedTifFileName then
        Ok cachedTifFileName
    else
        // download the tile
        let tileUrl = tileId |> worldCoverTileDownloadUrl

        Log.debug
            $"Downloading WorldCover tile {tileId.FormatLat2Lon3} from {tileUrl}..."

        downloadFile tileUrl cachedTifFileName |> Ok



let ensureWorldCoverFiles
    cacheDir
    (bounds: LonLatBounds)
    : Result<DemTileId list, string> =
    Log.info "Ensuring all needed WorldCover files are there..."

    let geoJsonFile = ensureGeoJsonFile cacheDir fileExists downloadFile

    let allAvailableFiles = listAllAvailableFiles openFileToRead geoJsonFile

    let filesNeeded = bounds |> boundsToWorldCoverFiles |> Seq.toList

    let availableFilesNeeded =
        filesNeeded
        |> List.filter (fun tileId ->
            allAvailableFiles
            |> Seq.exists (fun availableTileId -> availableTileId = tileId))

    let filesResults =
        availableFilesNeeded
        |> List.map (fun tileId ->
            tileId,
            ensureWorldCoverFile cacheDir fileExists downloadFile tileId)

    let filesErrors =
        filesResults
        |> List.choose (fun (_, result) ->
            match result with
            | Ok _ -> None
            | Error message -> Some message)

    match filesErrors with
    | [] -> Result.Ok availableFilesNeeded
    | _ -> Result.Error(String.concat "\n" filesErrors)


// todo 0: problem: WorldCover file covers not a single 1x1 degree tile, but
//   a 3x3 degree tile, so here we have a mix up between a "file tile" and a
//   a 1x1 degree "geo tile"

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
        Array.zeroCreate (WorldCoverTileSize * WorldCoverTileSize)

    // the size of an individual TIFF tile (not geographic tile, but a tile
    // in terms of tiling a TIFF raster into small quadratic pieces)
    let tiffTileWidth = unbox (tiff.GetField(TiffTag.TILEWIDTH).[0].Value)
    let tiffTileHeight = unbox (tiff.GetField(TiffTag.TILELENGTH).[0].Value)

    // the memory size (in bytes) of a single TIFF tile
    let tiffTileBufferSize = tiff.TileSize()
    let tiffTileBuffer = Array.zeroCreate<byte> tiffTileBufferSize

    // for each TIFF tile
    for tiffTileY in [ 0..tiffTileHeight..WorldCoverTileSize ] do
        // Y coordinate (row) of this TIFF tile in the grid of TIFF tiles
        let tileGridRow = tiffTileY / tiffTileHeight

        // minimum/corner Y cell coordinate of the TIFF tile in the WorldCover
        // grid of cells
        let tileMinY = tileGridRow * tiffTileHeight

        for tiffTileX in [ 0..tiffTileWidth..WorldCoverTileSize ] do
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

            // todo 100: not exactly optimal way to copy from one array to another
            for i in 0 .. tiffTileBufferSize - 1 do
                let value = tiffTileBuffer.[i]

                // local coordinates of the pixel within the TIFF tile
                let tiffTileLocalX = i % tiffTileWidth
                let tiffTileLocalY = i / tiffTileWidth

                // coordinates of the pixel within the heights array
                let heightsArrayX = tileMinX + tiffTileLocalX
                let heightsArrayY = tileMinY + tiffTileLocalY

                // index of the pixel within the heights array
                let index = heightsArrayY * WorldCoverTileSize + heightsArrayX

                // copy the pixel to the heights array only if it fits within
                // the array (some TIFF tiles may be partially outside the
                // requested area)
                if
                    heightsArrayX >= 0
                    && heightsArrayY >= 0
                    && heightsArrayX < WorldCoverTileSize
                    && heightsArrayY < WorldCoverTileSize
                then
                    worldCoverData.[index] <- int16 value

    let cellMinX, cellMinY =
        tileMinCell
            WorldCoverTileSize
            { Level = { Value = 0 }
              TileX = worldCoverTileId.TileX
              TileY = worldCoverTileId.TileY }

    HeightsArray(
        cellMinX,
        cellMinY,
        WorldCoverTileSize,
        WorldCoverTileSize,
        HeightsArrayDirectImport worldCoverData
    )
