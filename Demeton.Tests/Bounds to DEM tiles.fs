module Demeton.Tests.``Bounds to DEM tiles``

open Demeton

open FsUnit
open Xunit

[<Fact>]
let ``Two tiles are equal``() =
    { Lon = 10; Lat = 20 } |> should equal { Lon = 10; Lat = 20 }

[<Fact>]
let ``When bounds cover just a single tile``() =
    let bounds = { 
        MinLon = 10.1; MinLat = 20.1; MaxLon = 10.2; MaxLat = 20.2 }
    let tiles = bounds |> boundsToTiles
    tiles |> should equal [ { Lon = 10; Lat = 20 } ]

[<Fact>]
let ``When bounds cover multiple tiles``() =
    let bounds = { 
        MinLon = 10.1; MinLat = 20.1; MaxLon = 11.2; MaxLat = 21.2 }
    let tiles = bounds |> boundsToTiles
    tiles |> should equal [ 
        { Lon = 10; Lat = 20 } 
        { Lon = 11; Lat = 20 } 
        { Lon = 10; Lat = 21 } 
        { Lon = 11; Lat = 21 } 
    ]
