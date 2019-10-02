module Demeton.Tests.``Bounds to DEM tiles``

open Demeton.Geometry.Common
open Demeton.Srtm.Types
open Demeton.Srtm.Funcs

open FsUnit
open Xunit

[<Fact>]
let ``Two tiles are equal``() =
    { Lon = SrtmLongitude.fromInt 10; Lat = SrtmLatitude.fromInt 20 } 
    |> should equal { 
        Lon = SrtmLongitude.fromInt 10; Lat = SrtmLatitude.fromInt 20 }

[<Fact>]
let ``When bounds cover just a single tile``() =
    let bounds = { 
        MinLon = 10.1; MinLat = 20.1; MaxLon = 10.2; MaxLat = 20.2 }
    let tiles = bounds |> boundsToTiles
    tiles |> should equal [ 
        { Lon = SrtmLongitude.fromInt 10; Lat = SrtmLatitude.fromInt 20 } ]

[<Fact>]
let ``When bounds cover multiple tiles``() =
    let bounds = { 
        MinLon = 10.1; MinLat = 20.1; MaxLon = 11.2; MaxLat = 21.2 }
    let tiles = bounds |> boundsToTiles
    tiles |> should equal [ 
        { Lon = SrtmLongitude.fromInt 10; Lat = SrtmLatitude.fromInt 20 } 
        { Lon = SrtmLongitude.fromInt 11; Lat = SrtmLatitude.fromInt 20 } 
        { Lon = SrtmLongitude.fromInt 10; Lat = SrtmLatitude.fromInt 21 } 
        { Lon = SrtmLongitude.fromInt 11; Lat = SrtmLatitude.fromInt 21 } 
    ]
