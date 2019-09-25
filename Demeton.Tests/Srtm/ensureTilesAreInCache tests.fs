module Demeton.Tests.``ensureTilesAreInCache tests``

open Demeton
open Demeton.Srtm.Types
open Demeton.Srtm.Funcs

open FsUnit
open Xunit

[<Fact>]
let ``Returns empty list of tile files for empty list of tiles``() =
    let localCacheDir = @"d:\dem"

    localCacheDir |> ensureTilesAreInCache [] |> should be Empty

[<Fact(Skip="todo next:")>]
let ``When tile is already in the cache, do nothing``() =
    let localCacheDir = @"d:\dem"

    let tile = { Lon = SrtmLongitude.fromInt 2; Lat = SrtmLatitude.fromInt 3 }
    let results = localCacheDir |> ensureTilesAreInCache [ tile ]
    results 
    |> should equal [ 
        ({ Lon = SrtmLongitude.fromInt 2; Lat = SrtmLatitude.fromInt 3 }, 
            @"d:\dem\N03E002.hgt") ]
