module Tests.Srtm.``Bounds to SRTM tiles``

open Demeton.Dem.Types
open Demeton.Geometry.Common
open Demeton.Srtm.Funcs

open FsUnit
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Sample case 1`` () =
    let bounds =
        { MinLon = 0.
          MinLat = -2.
          MaxLon = 2.
          MaxLat = 0. }

    let tiles = boundsToTiles 3600 (SrtmLevel.fromInt 1) bounds

    tiles |> should equal [ srtmTileId 1 0 0 ]

[<Fact>]
let ``When bounds cover just a single tile inside 0,0 lon/lat, level 0`` () =
    let bounds =
        { MinLon = 0.1
          MinLat = 0.1
          MaxLon = 0.2
          MaxLat = 0.2 }

    let tiles = boundsToTiles 3600 (SrtmLevel.fromInt 0) bounds
    tiles |> should equal [ srtmTileId 0 0 -1 ]

[<Fact>]
let ``When bounds cover just a single tile, inside 0,0 lon/lat, level 1`` () =
    let bounds =
        { MinLon = 0.1
          MinLat = 0.1
          MaxLon = 0.2
          MaxLat = 0.2 }

    let tiles = boundsToTiles 3600 (SrtmLevel.fromInt 1) bounds
    tiles |> should equal [ srtmTileId 1 0 -1 ]

[<Fact>]
let ``When bounds cover just a single tile (case 2)`` () =
    let bounds =
        { MinLon = 0.1
          MinLat = -0.2
          MaxLon = 0.2
          MaxLat = -0.1 }

    let tiles = boundsToTiles 3600 (SrtmLevel.fromInt 0) bounds
    tiles |> should equal [ srtmTileId 0 0 0 ]

[<Fact>]
let ``When bounds cover just a single tile (case 3)`` () =
    let bounds =
        { MinLon = 10.1
          MinLat = 20.1
          MaxLon = 10.2
          MaxLat = 20.2 }

    let tiles = boundsToTiles 3600 (SrtmLevel.fromInt 0) bounds
    tiles |> should equal [ srtmTileId 0 10 -21 ]

[<Fact>]
let ``When bounds cover multiple tiles`` () =
    let bounds =
        { MinLon = 10.1
          MinLat = 20.1
          MaxLon = 11.2
          MaxLat = 21.2 }

    let tiles = boundsToTiles 3600 (SrtmLevel.fromInt 0) bounds

    tiles
    |> should
        equal
        [ srtmTileId 0 10 -22
          srtmTileId 0 11 -22
          srtmTileId 0 10 -21
          srtmTileId 0 11 -21 ]

[<Fact>]
let ``Supports calculating needed tiles when level higher than 0 is needed``
    ()
    =
    let bounds =
        { MinLon = 10.1
          MinLat = 19.1
          MaxLon = 11.2
          MaxLat = 21.2 }

    let tiles = boundsToTiles 3600 (SrtmLevel.fromInt 2) bounds
    test <@ tiles = [ srtmTileId 2 2 -6; srtmTileId 2 2 -5 ] @>

[<Fact>]
let ``Correctly calculates tiles lon/lat bounds`` () =
    let tileSize = 10
    let level = SrtmLevel.fromInt 1

    let tile = srtmTileId 1 0 0
    let bounds = tile |> tileLonLatBounds tileSize

    test
        <@
            bounds = { MinLon = 0.
                       MinLat = -2.
                       MaxLon = 2.
                       MaxLat = 0. }
        @>

    test <@ bounds |> boundsToTiles tileSize level = [ tile ] @>

    let tile = srtmTileId 1 0 -1
    let bounds = tile |> tileLonLatBounds tileSize

    test
        <@
            bounds = { MinLon = 0.
                       MinLat = 0.
                       MaxLon = 2.
                       MaxLat = 2. }
        @>

    test <@ bounds |> boundsToTiles tileSize level = [ tile ] @>

    let tile = srtmTileId 1 7 -24
    let bounds = tile |> tileLonLatBounds tileSize

    test
        <@
            bounds = { MinLon = 14.
                       MinLat = 46.
                       MaxLon = 16.
                       MaxLat = 48. }
        @>

    test <@ bounds |> boundsToTiles tileSize level = [ tile ] @>
