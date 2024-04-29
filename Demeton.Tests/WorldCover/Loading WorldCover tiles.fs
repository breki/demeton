module Tests.WorldCover.Loading_WorldCover_tiles

open System.IO
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
let listAllAvailableTiles (openFileToRead: FileWriter) (geoJsonFile: string) =
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
            properties["ll_tile"].Value<string>())

    | Error e -> failwith e.Exception.Message

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
    let geonJsonUrl =
        $"{s3UrlPrefix}/v100/2020/esa_worldcover_2020_grid.geojson"

    let geoJsonFile =
        downloadFile geonJsonUrl @"c:\temp\esa_worldcover_2020_grid.geojson"

    let allAvailableTiles = listAllAvailableTiles openFileToRead geoJsonFile

    test <@ allAvailableTiles |> Seq.length = 2631 @>


// todo 8: we need to calculate the WorldCover tile for a given lat/lon

// todo 9: we need to calculate the list of tiles needed for given bounds
