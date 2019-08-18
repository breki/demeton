module Demeton.Tests.``ensureTilesAreInCache tests``

open Demeton
open Demeton.SrtmTypes

open FsUnit
open Xunit

[<Fact>]
let ``Returns empty list of tile files for empty list of tiles``() =
    let localCacheDir = @"d:\dem";

    localCacheDir |> Srtm.ensureTilesAreInCache [] |> should be Empty

[<Fact(Skip="todo next:")>]
let ``When tile is already in the cache, do nothing``() =
    let localCacheDir = @"d:\dem";

    let tile = { Lon = 2; Lat = 3 }
    let results = localCacheDir |> Srtm.ensureTilesAreInCache [ tile ]
    results 
    |> should equal [ ({ Lon = 2; Lat = 3 }, @"d:\dem\N03E002.hgt") ]

   