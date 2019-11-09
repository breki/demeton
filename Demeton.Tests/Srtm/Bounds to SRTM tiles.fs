module Demeton.Tests.``Bounds to SRTM tiles``

open Demeton.Geometry.Common
open Demeton.Srtm.Types
open Demeton.Srtm.Funcs

open FsUnit
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Two tiles are equal``() =
    { Level = 0; Lon = SrtmLongitude.fromInt 10; Lat = SrtmLatitude.fromInt 20 } 
    |> should equal { 
        Level = 0; 
        Lon = SrtmLongitude.fromInt 10; Lat = SrtmLatitude.fromInt 20 }

[<Fact>]
let ``When bounds cover just a single tile``() =
    let bounds = { 
        MinLon = 10.1; MinLat = 20.1; MaxLon = 10.2; MaxLat = 20.2 }
    let tiles = boundsToTiles bounds 0
    tiles |> should equal [ 
        { Level = 0; 
        Lon = SrtmLongitude.fromInt 10; Lat = SrtmLatitude.fromInt 20 } ]

[<Fact>]
let ``When bounds cover multiple tiles``() =
    let bounds = { 
        MinLon = 10.1; MinLat = 20.1; MaxLon = 11.2; MaxLat = 21.2 }
    let tiles = boundsToTiles bounds 0
    tiles |> should equal [ 
        { Level = 0; 
        Lon = SrtmLongitude.fromInt 10; Lat = SrtmLatitude.fromInt 20 } 
        { Level = 0; Lon = SrtmLongitude.fromInt 11; Lat = SrtmLatitude.fromInt 20 } 
        { Level = 0; Lon = SrtmLongitude.fromInt 10; Lat = SrtmLatitude.fromInt 21 } 
        { Level = 0; Lon = SrtmLongitude.fromInt 11; Lat = SrtmLatitude.fromInt 21 } 
    ]

[<Fact>]
let ``Supports calculating needed tiles when level higher than 0 is needed``() =
    let bounds = { 
        MinLon = 10.1; MinLat = 19.1; MaxLon = 11.2; MaxLat = 21.2 }
    let tiles = boundsToTiles bounds 2
    test <@ tiles = [ 
            { Level = 2; 
            Lon = SrtmLongitude.fromInt 8; Lat = SrtmLatitude.fromInt 16 } 
            { Level = 2; 
            Lon = SrtmLongitude.fromInt 8; Lat = SrtmLatitude.fromInt 20 } 
        ] @>
