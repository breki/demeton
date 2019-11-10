module Demeton.Tests.``Bounds to SRTM tiles``

open Demeton.Geometry.Common
open Demeton.Srtm.Types
open Demeton.Srtm.Funcs

open FsUnit
open Xunit
open Swensen.Unquote
open Tests.Srtm.SrtmHelper

[<Fact>]
let ``Two tiles are equal``() =
    srtmTileCoords 0 10 20
    |> should equal (srtmTileCoords 0 10 20)

[<Fact>]
let ``When bounds cover just a single tile``() =
    let bounds = { 
        MinLon = 10.1; MinLat = 20.1; MaxLon = 10.2; MaxLat = 20.2 }
    let tiles = boundsToTiles bounds (SrtmLevel.fromInt 0)
    tiles |> should equal [ srtmTileCoords 0 10 20 ]

[<Fact>]
let ``When bounds cover multiple tiles``() =
    let bounds = { 
        MinLon = 10.1; MinLat = 20.1; MaxLon = 11.2; MaxLat = 21.2 }
    let tiles = boundsToTiles bounds (SrtmLevel.fromInt 0)
    tiles |> should equal [ 
        srtmTileCoords 0 10 20
        srtmTileCoords 0 11 20
        srtmTileCoords 0 10 21
        srtmTileCoords 0 11 21
    ]

[<Fact>]
let ``Supports calculating needed tiles when level higher than 0 is needed``() =
    let bounds = { 
        MinLon = 10.1; MinLat = 19.1; MaxLon = 11.2; MaxLat = 21.2 }
    let tiles = boundsToTiles bounds (SrtmLevel.fromInt 2)
    test <@ tiles = [ 
        srtmTileCoords 2 8 16
        srtmTileCoords 2 8 20
        ] @>
