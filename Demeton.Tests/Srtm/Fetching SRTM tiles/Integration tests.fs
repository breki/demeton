﻿module Tests.Srtm.``Fetching SRTM tiles``.``Integration tests``

open Demeton.Srtm.Fetch

open System

open Xunit
open Swensen.Unquote
open TestHelp
open Tests.Srtm.SrtmHelper

let srtmDir = "srtm"
let cacheDir = Environment.GetEnvironmentVariable("SRTM_CACHE")


let convertFromHgt tile = invalidOp "todo"

[<Fact(Skip="todo currently not working")>]
[<Trait("Category","slow")>]
let ``Supports fetching already cached tile``() =
    let finalState =
        initializeProcessingState (srtmTileCoords 0 15 46)
        |> processCommandStack 
            (determineTileStatus srtmDir cacheDir FileSys.fileExists) 
            convertFromHgt
            (createFromLowerTiles cacheDir FileSys.openFileToRead)
    let result = 
        finalState 
        |> (finalizeFetchSrtmTileProcessing 
                (readPngTile cacheDir FileSys.openFileToRead))
    test <@ result |> isOk @>

[<Fact(Skip="todo currently not working")>]
[<Trait("Category","slow")>]
let ``Supports fetching higher level tile by creating it from lower level ones``() =
    let finalState =
        initializeProcessingState (srtmTileCoords 1 14 46)
        |> processCommandStack 
            (determineTileStatus srtmDir cacheDir FileSys.fileExists) 
            convertFromHgt
            (createFromLowerTiles cacheDir FileSys.openFileToRead)
    let result = 
        finalState
        |> (finalizeFetchSrtmTileProcessing 
                (readPngTile cacheDir FileSys.openFileToRead))
    test <@ result |> isOk @>
