module Tests.Srtm.``Fetching SRTM tiles``.``Integration tests``

open Demeton.Srtm.Fetch
open Demeton.Srtm.Funcs
open Demeton.Srtm.Png
open Demeton.Srtm.Downsampling

open System

open Xunit
open Swensen.Unquote
open TestHelp

let srtmDir = "srtm"
let cacheDir = Environment.GetEnvironmentVariable("SRTM_CACHE")

let readPngTile: SrtmPngTileReader = fun fileName ->
    decodeSrtmTileFromPngFile FileSys.openFileToRead fileName

let determineTileStatus = 
    determineTileStatus srtmDir cacheDir FileSys.fileExists

let writeHeightsArrayToPng =
    writeHeightsArrayIntoPngFile
        FileSys.ensureDirectoryExists
        FileSys.openFileToWrite

let openHgtStream = openZippedHgtFileStream FileSys.openZipFileEntry

let convertToPng =
    convertZippedHgtTileToPng 
        openHgtStream createSrtmTileFromStream writeHeightsArrayToPng

let constructHigherLevelTile =
    constructHigherLevelTileHeightsArray 3600 cacheDir readPngTile

let heightsArrayToPng =
    writeHeightsArrayIntoPngFile
        FileSys.ensureDirectoryExists
        FileSys.openFileToWrite

let writeTileToCache = 
    writeSrtmTileToLocalCache 
        cacheDir
        FileSys.ensureDirectoryExists
        heightsArrayToPng
        FileSys.openFileToWrite

[<Fact>]
[<Trait("Category","slow")>]
let ``Supports fetching already cached tile``() =
    // Skip this tes|t if the SRTM_DIR environment variable is not set.
    // This is a current workaround for GitHub action not getting the
    // variable set.
    match cacheDir with
    | null -> ignore()
    | _ ->
        let finalState =
            initializeProcessingState (srtmTileId 0 15 -46)
            |> processCommandStack 
                cacheDir
                srtmDir
                determineTileStatus
                convertToPng
                constructHigherLevelTile
                writeTileToCache
        let result = 
            finalState
            |> finalizeFetchSrtmTileProcessing cacheDir readPngTile
        test <@ result |> isOk @>

[<Fact(Skip="todo currently not working")>]
[<Trait("Category","slow")>]
let ``Supports fetching higher level tile by creating it from lower level ones``() =
    let finalState =
        initializeProcessingState (srtmTileId 1 14 -46)
        |> processCommandStack 
            cacheDir
            srtmDir
            determineTileStatus
            convertToPng
            constructHigherLevelTile
            writeTileToCache
    let result = 
        finalState
        |> finalizeFetchSrtmTileProcessing cacheDir readPngTile
    test <@ result |> isOk @>

