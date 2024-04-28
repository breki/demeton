module Tests.WorldCover.Loading_WorldCover_tiles

open System.Dynamic
open System.IO
open FileSys

open FsUnit
open Newtonsoft.Json
open Xunit
open Swensen.Unquote

let year = 2021
let version = "v200" // year 2021
let s3UrlPrefix = "https://esa-worldcover.s3.eu-central-1.amazonaws.com"


[<Fact>]
let ``Icebreaker`` () =
    let geonJsonUrl =
        $"{s3UrlPrefix}/v100/2020/esa_worldcover_2020_grid.geojson"

    let geoJsonFile =
        downloadFile geonJsonUrl @"c:\temp\esa_worldcover_2020_grid.geojson"

    // read geoJson file
    let geoJsonObj =
        match openFileToRead geoJsonFile with
        | Ok stream ->
            use reader = new StreamReader(stream)
            use jsonReader = new JsonTextReader(reader)
            let serializer = JsonSerializer()
            serializer.Deserialize<ExpandoObject>(jsonReader)
        | Error e -> failwith e.Exception.Message

    // todo 0: fetch the set of all available WorldCover tiles
    // features[0].properties.ll_tile S54E168

    // todo 4: the downloaded geojson file should be cached and retrieved from
    //   the cache if it exists

    // todo 8: we need to calculate the WorldCover tile for a given lat/lon

    // todo 9: we need to calculate the list of tiles needed for given bounds

    test <@ true @>
