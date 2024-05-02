module Tests.WorldCover.Loading_WorldCover_tiles

open System
open System.IO
open Demeton.Geometry.Common
open FileSys

open FsUnit
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Xunit
open Swensen.Unquote
open TestHelp

let year = 2021
let version = "v200" // year 2021
let s3UrlPrefix = "https://esa-worldcover.s3.eu-central-1.amazonaws.com"
let geoJsonUrl = $"{s3UrlPrefix}/v100/2020/esa_worldcover_2020_grid.geojson"

[<Literal>]
let WorldCoverDirName = "WorldCover"


type WorldCoverTileName = string

type WorldCoverTileId =
    {
        /// <summary>
        /// Represents the X coordinate of the WorldCover tile.
        /// This is an integer value which is positive for tiles east of the
        /// Greenwich meridian and negative for tiles west of it.
        /// </summary>
        TileX: int

        /// <summary>
        /// Represents the Y coordinate of the WorldCover tile.
        /// This is an integer value which is positive for tile south of the
        /// Equator and negative for tiles north of it.
        /// </summary>
        TileY: int
    }

    member this.TileName: WorldCoverTileName =
        let latSign =
            match this.TileY with
            | x when x >= 0 -> 'N'
            | _ -> 'S'

        let lonSign =
            match this.TileX with
            | x when x >= 0 -> 'E'
            | _ -> 'W'

        $"%c{latSign}%03d{abs this.TileX}%c{lonSign}%03d{abs this.TileY}"




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
    let cellsPerDegree = 3600
    let tileSize = 3600
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



[<Fact>]
let ``Download WorldCover geoJSON file if it is not already cached`` () =
    let cacheDir = "cache"

    let geoJsonCachedFileName = worldCoverGeoJsonCachedFileName cacheDir

    let fileExists =
        function
        | fileName when fileName = geoJsonCachedFileName -> false
        | _ -> fail "Unexpected file name"

    let mutable downloadFileCalls = 0

    let downloadFile (url: string) (fileName: string) =
        match url, fileName with
        | url, fileName when
            url = geoJsonUrl && fileName = geoJsonCachedFileName
            ->
            downloadFileCalls <- downloadFileCalls + 1
            geoJsonCachedFileName
        | _ -> fail "Unexpected URL or file name"


    let resultingFileName = ensureGeoJsonFile "cache" fileExists downloadFile

    test <@ resultingFileName = geoJsonCachedFileName @>
    test <@ downloadFileCalls = 1 @>


[<Fact>]
let ``Skip downloading WorldCover geoJSON file if it is already cached`` () =
    let cacheDir = "cache"

    let geoJsonCachedFileName = worldCoverGeoJsonCachedFileName cacheDir

    let fileExists =
        function
        | fileName when fileName = geoJsonCachedFileName -> true
        | _ -> fail "Unexpected file name"

    let downloadFile (url: string) (fileName: string) =
        fail "Should not be called"


    let resultingFileName = ensureGeoJsonFile "cache" fileExists downloadFile

    test <@ resultingFileName = geoJsonCachedFileName @>


[<Fact>]
let ``Can fetch the list of all available WorldCover tiles`` () =
    let cacheDir = "cache"

    let geoJsonFile = ensureGeoJsonFile cacheDir fileExists downloadFile

    let allAvailableTiles = listAllAvailableTiles openFileToRead geoJsonFile

    test <@ allAvailableTiles |> Seq.length = 2631 @>


// todo 8: we need to calculate the WorldCover tile for a given lat/lon, but
//   we have to take into account available tiles only. We should also take
//   into account the fact that WorldCover tiles are bigger than SRTM ones.

[<Fact>]
let ``Correctly calculates the WorldCover tiles needed for a given boundary``
    ()
    =
    let bounds =
        { MinLon = 46.1
          MinLat = 6.9
          MaxLon = 49.9
          MaxLat = 10.1 }

    test
        <@
            boundsToWorldCoverTiles bounds |> Set.ofSeq = set
                [ { TileX = 45; TileY = -6 }
                  { TileX = 48; TileY = -6 }
                  { TileX = 45; TileY = -9 }
                  { TileX = 48; TileY = -9 } ]
        @>



// todo 9: we need to calculate the list of tiles needed for given bounds
