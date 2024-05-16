module Demeton.WorldCover.Funcs

open System
open System.IO
open Demeton.DemTypes
open Demeton.Geometry.Common
open Demeton.WorldCover.Types
open FileSys
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open BitMiracle.LibTiff.Classic



/// <summary>
/// Parses a WorldCover tile name and returns a corresponding WorldCoverTileId.
/// </summary>
/// <param name="tileName">The name of the WorldCover tile to parse.</param>
/// <returns>
/// A WorldCoverId that represents the parsed WorldCover tile.
/// </returns>
let parseTileName (tileName: WorldCoverTileName) : WorldCoverTileId =
    let latitudeCharSign = tileName.[0]

    let latitudeSign =
        match latitudeCharSign with
        | 'N' -> 1
        | 'S' -> -1
        | _ ->
            raise (
                InvalidOperationException
                    $"Invalid SRTM tile ID: '%s{tileName}'"
            )

    let longitudeCharSign = tileName.[3]

    let longitudeSign =
        match longitudeCharSign with
        | 'W' -> -1
        | 'E' -> 1
        | _ ->
            raise (
                InvalidOperationException
                    $"Invalid SRTM tile ID: '%s{tileName}'"
            )

    let latitudeStr = tileName.[1..2]
    let latitudeInt = Int32.Parse latitudeStr * latitudeSign

    let longitudeStr = tileName.[4..6]
    let longitudeInt = Int32.Parse longitudeStr * longitudeSign

    { TileX = longitudeInt
      TileY = latitudeInt }


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
/// List all available WorldCover tiles by reading the specified WorldCover
/// geoJSON file.
/// </summary>
let listAllAvailableTiles
    (openFileToRead: FileWriter)
    (geoJsonFile: string)
    : WorldCoverTileId seq =
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
/// Given a bounding box, returns a sequence of WorldCover tiles that cover it.
/// </summary>
let boundsToWorldCoverTiles (bounds: LonLatBounds) : WorldCoverTileId seq =
    let degreesPerTile = 3

    let minTileX =
        (bounds.MinLon / float degreesPerTile) |> floor |> int |> (*) 3

    let minTileY = (bounds.MaxLat / float degreesPerTile) |> ceil |> int
    let minTileY = -(minTileY - 1 |> (*) degreesPerTile)
    let maxTileX = (bounds.MaxLon / float degreesPerTile) |> ceil |> int
    let maxTileX = maxTileX - 1 |> (*) degreesPerTile

    let maxTileY =
        -(bounds.MinLat / float degreesPerTile |> floor |> int |> (*) 3)

    seq {
        for tileY in [ minTileY..degreesPerTile..maxTileY ] do
            for tileX in [ minTileX..degreesPerTile..maxTileX ] do
                yield { TileX = tileX; TileY = tileY }
    }

/// <summary>
/// Returns the download URL for the given WorldCover tile.
/// </summary>
let worldCoverTileDownloadUrl (tileId: WorldCoverTileId) : string =
    sprintf
        "%s/%s/%i/map/ESA_WorldCover_10m_%i_%s_%s_Map.tif"
        WorldCoverS3Domain
        WorldCoverVersion
        WorldCoverYear
        WorldCoverYear
        WorldCoverVersion
        tileId.TileName

/// <summary>
/// Returns the path to the cached TIFF file for the given AW3D tile.
/// </summary>
let worldCoverTileCachedTifFileName cacheDir (tileId: WorldCoverTileId) =
    Path.Combine(cacheDir, WorldCoverDirName, $"{tileId.TileName}.tif")



/// <summary>
/// Ensures the specified AW3D tile TIFF file is available in the cache
/// directory, downloading it if necessary.
/// </summary>
let ensureWorldCoverTile cacheDir fileExists downloadFile tileId =
    let cachedTifFileName = worldCoverTileCachedTifFileName cacheDir tileId

    if fileExists cachedTifFileName then
        Ok cachedTifFileName
    else
        // download the tile
        let tileUrl = tileId |> worldCoverTileDownloadUrl

        Log.debug
            $"Downloading WorldCover tile {tileId.TileName} from {tileUrl}..."

        downloadFile tileUrl cachedTifFileName |> Ok


/// <summary>
/// Reads a WorldCover tile file and returns the heights array for it.
/// </summary>
/// <remarks>
/// The function accepts the tile coordinates of the lower left corner
/// of the WorldCover raster file.
/// </remarks>
let readWorldCoverTile
    cacheDir
    (worldCoverTileId: WorldCoverTileId)
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
        for tiffTileX in [ 0..tiffTileWidth..WorldCoverTileSize ] do
            /// 0-based byte offset in buffer at which to begin storing read
            /// and decoded bytes
            let offset = 0
            /// z-coordinate of the pixel within a tile to be read and decoded
            let tileZ = 0
            /// The zero-based index of the sample plane. The plane parameter is
            /// used only if data are organized in separate planes
            /// (PLANARCONFbIG = SEPARATE). In other cases the value is ignored.
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

            // todo 100: not exactly optimal way to copy from one array to another
            for i in 0 .. tiffTileBufferSize - 1 do
                let value = tiffTileBuffer.[i]

                // local coordinates of the pixel within the TIFF tile
                let tiffTileLocalX = i % tiffTileWidth
                let tiffTileLocalY = i / tiffTileWidth

                // coordinates of the pixel within the heights array
                let heightsArrayX = actualTileX + tiffTileLocalX

                let heightsArrayY = actualTileY + tiffTileLocalY

                // index of the pixel within the heights array
                let index = heightsArrayY * WorldCoverTileSize + heightsArrayX

                worldCoverData.[index] <- int16 value

    let cellMinX, cellMinY =
        Demeton.Srtm.Funcs.tileMinCell
            WorldCoverTileSize
            { Level = { Value = 1 }
              TileX = worldCoverTileId.TileX
              TileY = worldCoverTileId.TileY }

    HeightsArray(
        cellMinX,
        cellMinY,
        WorldCoverTileSize,
        WorldCoverTileSize,
        HeightsArrayDirectImport worldCoverData
    )
