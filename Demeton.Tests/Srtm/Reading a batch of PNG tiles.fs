﻿module Tests.Srtm.``Reading a batch of PNG tiles``

open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Demeton.Srtm.Funcs
open Demeton.Srtm.Png
open Xunit
open Swensen.Unquote
open TestHelp

let private cacheDir = "some-cache"

let private tileHeights =
    HeightsArray(1, 2, 3, 4, HeightsArrayInitializer1D(fun _ -> DemHeightNone))

let private decodePng: DemPngTileReader = fun _ _ -> Ok tileHeights

// fails only on certain tiles
let private decodePngWithFailure: DemPngTileReader =
    fun tileId _ ->
        match tileId.TileX = 3 with
        | false -> Ok tileHeights
        | true -> Error "some error"


[<Fact>]
let ``All children tiles have been read successfully`` () =
    let tiles = [ demTileId 1 2 -4 ]

    let result = readPngTilesBatch cacheDir decodePng tiles
    test <@ result |> isOk @>

[<Fact>]
let ``Reading of some children tiles have failed`` () =
    let tiles = [ demTileId 1 2 4; demTileId 1 3 4; demTileId 1 4 4 ]

    let result = readPngTilesBatch cacheDir decodePngWithFailure tiles
    test <@ result |> isError @>
