module Tests.Srtm.``Fetching SRTM tiles``.``Integration tests``

open Demeton.Srtm.Types
open Demeton.Srtm.Funcs
open Demeton.Srtm.Png
open Demeton.Srtm.Fetch
open Demeton.DemTypes

open System

open Xunit
open Swensen.Unquote
open TestHelp
open Tests.Srtm.SrtmHelper

let srtmDir = "srtm"
let cacheDir = Environment.GetEnvironmentVariable("SRTM_CACHE")

let determineTileStatus
    srtmDir
    localCacheDir
    (fileExists: FileSys.FileExistsChecker)
    (tile: SrtmTileCoords) = 

    let tilePngExistsInLocalCache tile = 
        tile
        |> toLocalCacheTileFileName localCacheDir
        |> fileExists
    
    let tileNoneFileExistsInLocalCache tile = 
        tile
        |> toLocalCacheTileFileName localCacheDir
        |> Pth.extension ".none"
        |> fileExists

    let checkSrtmDirTileStatus() =
        checkSrtmDirTileStatus srtmDir fileExists tile.Lon tile.Lat

    let localCacheStatus =
        determineLocalCacheTileStatus 
            tile.Level
            (tilePngExistsInLocalCache tile)
            (Lazy<bool>(fun () -> tileNoneFileExistsInLocalCache tile))

    decideSrtmTileStatus
        tile.Level
        localCacheStatus
        (Lazy<SrtmDirTileStatus>(fun () -> checkSrtmDirTileStatus()))

let convertFromHgt tile = invalidOp "todo"

let readPngTile localCacheDir openFileToRead tile =
    tile 
    |> toLocalCacheTileFileName localCacheDir
    |> decodeSrtmTileFromPngFile openFileToRead

let readPngTilesBatch 
    localCacheDir 
    openFileToRead 
    (tiles: SrtmTileCoords list)
    : Result<HeightsArray list, string> =
     
    let readPngTile readingState tile =
        match readingState with
        | Ok heightsArrays ->
            match readPngTile localCacheDir openFileToRead tile with
            | Ok heightsArray -> Ok (heightsArray :: heightsArrays)
            | Error message -> Error message
        | Error message -> Error message

    tiles |> List.fold readPngTile (Ok [])

let createFromLowerTiles 
    localCacheDir openFileToRead parentTile children = 
   
    let lowerTilesReadResult = 
        children |> readPngTilesBatch localCacheDir openFileToRead

    match lowerTilesReadResult with
    | Ok lowerTilesHeightsArrays ->
        let mergedHeightsArrayMaybe =
            lowerTilesHeightsArrays |> Demeton.Dem.merge
        mergedHeightsArrayMaybe |> Ok
    | Error message -> Error message

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

