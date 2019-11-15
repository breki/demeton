module Tests.Srtm.``Fetching SRTM tiles``.``Integration tests``

open Demeton.Srtm.Fetch
open Demeton.Srtm.Funcs
open Demeton.Srtm.Png

open System

open Xunit
open Swensen.Unquote
open TestHelp
open Tests.Srtm.SrtmHelper

let srtmDir = "srtm"
let cacheDir = Environment.GetEnvironmentVariable("SRTM_CACHE")

let readPngTile: SrtmPngTileReader = fun fileName ->
    decodeSrtmTileFromPngFile FileSys.openFileToRead fileName

let determineTileStatus = 
    determineTileStatus srtmDir cacheDir FileSys.fileExists

let writeTileToPng =
    encodeHeightsArrayIntoPngFile
        FileSys.ensureDirectoryExists
        FileSys.openFileToWrite

let convertToPng =
    convertZippedHgtTileToPng 
        FileSys.openZipFileEntry 
        createSrtmTileFromStream 
        writeTileToPng

let constructHigherLevelTile =
    constructHigherLevelTileHeightsArray 3600 cacheDir readPngTile

[<Fact(Skip="todo currently not working")>]
[<Trait("Category","slow")>]
let ``Supports fetching already cached tile``() =
    let finalState =
        initializeProcessingState (srtmTileCoords 0 15 46)
        |> processCommandStack 
            cacheDir
            srtmDir
            determineTileStatus
            convertToPng
            constructHigherLevelTile
    let result = 
        finalState
        |> finalizeFetchSrtmTileProcessing cacheDir readPngTile
    test <@ result |> isOk @>

[<Fact(Skip="todo currently not working")>]
[<Trait("Category","slow")>]
let ``Supports fetching higher level tile by creating it from lower level ones``() =
    let finalState =
        initializeProcessingState (srtmTileCoords 1 14 46)
        |> processCommandStack 
            cacheDir
            srtmDir
            determineTileStatus
            convertToPng
            constructHigherLevelTile
    let result = 
        finalState
        |> finalizeFetchSrtmTileProcessing cacheDir readPngTile
    test <@ result |> isOk @>

