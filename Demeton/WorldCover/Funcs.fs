module Demeton.WorldCover.Funcs

open System
open System.IO
open Demeton.Geometry.Common
open Demeton.WorldCover.Types
open FileSys
open Newtonsoft.Json
open Newtonsoft.Json.Linq



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
