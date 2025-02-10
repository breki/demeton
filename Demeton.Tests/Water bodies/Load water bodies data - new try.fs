module Tests.Water_bodies.Load_water_bodies_data_new_try


open FsUnit
open Xunit
open Swensen.Unquote
open Demeton.WorldCover.Fetch
open Demeton.WorldCover.WaterBodiesFetch
open FileSys
open Demeton.Dem.Funcs



[<Literal>]
let CacheDir = "cache"

[<Fact(Skip = "downloads a tile and breaks it down into subtiles, so it takes too long")>]
// [<Fact>]
let icebreaker () =
    let tileId = (demTileXYId 7 45)

    let availableWorldCoverTiles =
        ensureGeoJsonFile CacheDir fileExists downloadFile
        |> listAllAvailableFiles openFileToRead
        |> Set.ofSeq

    let result =
        loadWaterBodiesTileFromCache CacheDir availableWorldCoverTiles tileId
        |> makeNoneFileIfNeeded CacheDir
        |> extractWaterBodiesTileFromWorldCoverTileIfNeeded CacheDir

    match result with
    | CachedTileLoaded heightsArray -> test <@ heightsArray |> Option.isSome @>
    | _ -> Should.fail "Unexpected result"


[<Fact(Skip = "downloads a tile and breaks it down into subtiles, so it takes too long")>]
// [<Fact>]
let bugTest () =
    let tileId = demTileXYId -29 38

    let availableWorldCoverTiles =
        ensureGeoJsonFile CacheDir fileExists downloadFile
        |> listAllAvailableFiles openFileToRead
        |> Set.ofSeq

    let result =
        loadWaterBodiesTileFromCache CacheDir availableWorldCoverTiles tileId
        |> makeNoneFileIfNeeded CacheDir
        |> extractWaterBodiesTileFromWorldCoverTileIfNeeded CacheDir

    match result with
    | CachedTileLoaded heightsArray -> test <@ heightsArray |> Option.isSome @>
    | _ -> Should.fail "Unexpected result"
