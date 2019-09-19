module Demeton.Tests.``Commands tests``.``ImportSrtmTilesCommand tests``

open Demeton.Commands.ImportSrtmTilesCommand
open Demeton.GeometryTypes
open Demeton.SrtmTypes

open Xunit


[<Fact>]
let ``Imports all tiles within the specified boundaries``() =
    let tiles = [
        { Lon = SrtmLongitude.fromInt 15; Lat = SrtmLatitude.fromInt 45 }
        { Lon = SrtmLongitude.fromInt 16; Lat = SrtmLatitude.fromInt 46 }
    ]

    import tiles
