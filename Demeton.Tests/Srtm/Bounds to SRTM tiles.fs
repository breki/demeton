module Tests.Srtm.``Bounds to SRTM tiles``

open Demeton.Geometry.Common
open Demeton.Srtm.Types
open Demeton.Srtm.Funcs

open FsUnit
open Xunit
open Swensen.Unquote
open Tests.Srtm.SrtmHelper

[<Fact>]
let ``Two tiles are equal``() =
    srtmTileCoords 10 20 |> should equal (srtmTileCoords 10 20)

[<Fact>]
let ``When bounds cover just a single tile``() =
    let bounds = { 
        MinLon = 10.1; MinLat = 20.1; MaxLon = 10.2; MaxLat = 20.2 }
    let tiles = boundsToTiles 3600 (SrtmLevel.fromInt 0) bounds
    tiles |> should equal [ srtmTileId 0 10 -21 ]

[<Fact>]
let ``When bounds cover multiple tiles``() =
    let bounds = { 
        MinLon = 10.1; MinLat = 20.1; MaxLon = 11.2; MaxLat = 21.2 }
    let tiles = boundsToTiles 3600 (SrtmLevel.fromInt 0) bounds
    tiles |> should equal [ 
        srtmTileId 0 10 -22
        srtmTileId 0 11 -22
        srtmTileId 0 10 -21
        srtmTileId 0 11 -21
    ]

[<Fact>]
let ``Supports calculating needed tiles when level higher than 0 is needed``() =
    let bounds = { 
        MinLon = 10.1; MinLat = 19.1; MaxLon = 11.2; MaxLat = 21.2 }
    let tiles = boundsToTiles 3600 (SrtmLevel.fromInt 2) bounds
    test <@ tiles = [ srtmTileId 2 2 -6; srtmTileId 2 2 -5 ] @>
