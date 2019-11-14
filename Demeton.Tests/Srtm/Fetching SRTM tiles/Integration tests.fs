module Tests.Srtm.``Fetching SRTM tiles``.``Integration tests``

open Demeton.Srtm.Types
open Demeton.Srtm.Funcs
open Demeton.Srtm.Png
open Demeton.Srtm.Fetch
open Demeton.DemTypes

open System

open Xunit
open Swensen.Unquote
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

let createFromLowerTiles 
    localCacheDir openFile
    parentTile children = 

    let readPngTile tile =
        let tileFileName = 
            tile |> toLocalCacheTileFileName localCacheDir
        let heightsArrayMaybe = 
            decodeSrtmTileFromPngFile openFile tileFileName
        match heightsArrayMaybe with
        | Ok heightsArray -> heightsArray
        | Error message -> invalidOp (sprintf "todo handle: %s" message)
    
    let mergedHeightsArrayMaybe =
        children
        |> List.map (fun tile -> readPngTile tile)
        |> Demeton.Dem.merge

    match mergedHeightsArrayMaybe with
    | Some heightsArray -> heightsArray
    | None -> invalidOp "todo write .none file to the cache"

let fetchSrtmTile tile =
    let initialState =
        ([], [])
        |>
        newTileToProcess tile

    let (finalCommandStack, finalTilesStack) = 
        processCommandStack
            // all of the tiles are marked as cached
            (determineTileStatus srtmDir cacheDir FileSys.fileExists)
            convertFromHgt 
            (createFromLowerTiles cacheDir FileSys.openFileToRead)
            initialState

    // there should be no more commands in the stack
    test <@ finalCommandStack = [] @>
    // ensure all of the tiles are in the tiles stack
    test <@ finalTilesStack = [ Some tile ] @>

[<Fact>]
let ``Supports fetching already cached tile``() =
    fetchSrtmTile (srtmTileCoords 0 15 46)

[<Fact>]
[<Trait("Category","slow")>]
let ``Supports fetching higher level tile by creating it from lower level ones``() =
    fetchSrtmTile (srtmTileCoords 1 14 46)

