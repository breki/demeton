module Demeton.Srtm.Fetch

open Types
open Funcs
open Demeton.DemTypes
open System

type LocalCacheTileStatus =
    | NotCached
    | HigherLevelDoesNotExist
    | Cached

let determineLocalCacheTileStatus 
    (level: SrtmLevel)
    tilePngExistsInLocalCache
    (tileNoneFileExistsInLocalCache: Lazy<bool>)
    =
    match tilePngExistsInLocalCache with
    | true -> Cached
    | false -> 
        match level.Value with
        | 0 -> NotCached    
        | _ -> 
            match tileNoneFileExistsInLocalCache.Value with
            | true -> HigherLevelDoesNotExist
            | false -> NotCached

type SrtmDirTileStatus =
    | DoesNotExist
    | Exists

let checkSrtmDirTileStatus 
    srtmDir (fileExists: FileSys.FileExistsChecker)
    lon lat =

    { Level = SrtmLevel.fromInt 0; Lon = lon; Lat = lat }
    |> toZippedSrtmTileFileName srtmDir
    |> fileExists
    |> function
    | true -> Exists
    | false -> DoesNotExist

type SrtmTileStatus =
    | NotExists
    | NotCached
    | Cached

let decideSrtmTileStatus
    (level: SrtmLevel) 
    localCacheTileStatus 
    (srtmTileStatus: Lazy<SrtmDirTileStatus>) =

    match (level.Value, localCacheTileStatus) with
    | (_, LocalCacheTileStatus.Cached) -> Cached
    | (0, LocalCacheTileStatus.NotCached) ->
        match srtmTileStatus.Value with
        | SrtmDirTileStatus.DoesNotExist -> NotExists
        | SrtmDirTileStatus.Exists -> NotCached
    | (level, LocalCacheTileStatus.NotCached) when level > 0 -> NotCached
    | (level, LocalCacheTileStatus.HigherLevelDoesNotExist) when level > 0 -> 
        NotExists
    | _ -> invalidOp "bug: this should never happen"

type CreateFromLowerTiles = { 
    Parent: SrtmTileCoords; Children: SrtmTileCoords[] }

type TileProcessingCommand =
    | DetermineStatus of SrtmTileCoords
    | ConvertTileFromHgt of SrtmTileCoords 
    | CreateFromLowerTiles of CreateFromLowerTiles
    | Failure of string

type TileProcessingCommandStack = TileProcessingCommand list

type TileInStack = SrtmTileCoords option
type TilesStack = TileInStack list

type TileFetchingState = (TileProcessingCommandStack * TilesStack)

let newTileToProcess tile ((stack, tilesBuffer): TileFetchingState):
    TileFetchingState =
    ((DetermineStatus tile) :: stack, tilesBuffer)

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

// todo tests for listChildrenTiles that check edge cases
let listChildrenTiles tile =
    let childLevel = tile.Level.Value - 1
    let childLonLatDelta = pown 2 childLevel
    let childLon0 = tile.Lon.Value - childLonLatDelta
    let childLat0 = tile.Lat.Value - childLonLatDelta

    [|
        for lat in 1 .. 4 do
            for lon in 1 .. 4 do
                yield { 
                    Level = SrtmLevel.fromInt childLevel; 
                    Lon = SrtmLongitude.fromInt 
                        (childLon0 + (lon - 1) * childLonLatDelta)
                    Lat = SrtmLatitude.fromInt 
                        (childLat0 + (lat - 1) * childLonLatDelta) }
    |]

let convertFromHgt tile = invalidOp "todo"

let readPngTile 
    localCacheDir (decodeSrtmTileFromPngFile: SrtmPngTileReader) tile =
    tile 
    |> toLocalCacheTileFileName localCacheDir
    |> decodeSrtmTileFromPngFile

let readPngTilesBatch 
    localCacheDir 
    decodeSrtmTileFromPngFile 
    (tiles: SrtmTileCoords list)
    : Result<HeightsArray list, string> =
     
    let readPngTile readingState tile =
        match readingState with
        | Ok heightsArrays ->
            readPngTile localCacheDir decodeSrtmTileFromPngFile tile
            |> Result.map (fun heightsArray -> heightsArray :: heightsArrays)
        | Error message -> Error message

    tiles |> List.fold readPngTile (Ok [])

let fetchFirstNOfTiles tilesCount = List.splitAt tilesCount

let processNextCommand 
    localCacheDir
    determineTileStatus
    (convertFromHgt: SrtmTileCoords -> Result<unit, string>)
    readTilePngFile
    ((commandStack, tilesStack): TileFetchingState)
    : TileFetchingState =
    match commandStack with
    | [] -> ([], tilesStack)

    | DetermineStatus tile :: remainingCommands -> 
        match determineTileStatus tile with
        | NotExists -> (remainingCommands, None :: tilesStack)
        | Cached -> (remainingCommands, Some tile :: tilesStack)
        | NotCached when tile.Level.Value = 0 -> 
            ((ConvertTileFromHgt tile) :: remainingCommands, tilesStack)
        | NotCached ->
            let childrenTiles = listChildrenTiles tile
            let cmd = { Parent = tile; Children = childrenTiles }
            let childrenCommands = 
                childrenTiles |> Array.map DetermineStatus |> Array.toList
            let updatedStack = 
                List.append 
                    childrenCommands 
                    ((CreateFromLowerTiles cmd) :: remainingCommands)
            (updatedStack, tilesStack)

    | ConvertTileFromHgt tile :: remainingCommands ->
        match convertFromHgt tile with
        | Ok() -> (remainingCommands, Some tile :: tilesStack)
        | Error message -> (Failure message :: remainingCommands, tilesStack)

    | CreateFromLowerTiles parameters :: remainingCommands ->
        let tile = parameters.Parent
       
        // fetch the required number of tiles from the tiles stack
        let lowerTilesNumber = parameters.Children.Length
        let (lowerTiles, remainingTilesInStack) = 
            tilesStack |> fetchFirstNOfTiles lowerTilesNumber

        let lowerTilesWithoutNone =
            lowerTiles
            |> List.filter Option.isSome
            |> List.map Option.get

        lowerTilesWithoutNone
        |> readPngTilesBatch localCacheDir readTilePngFile
        |> Result.map Demeton.Dem.merge
        // after merging the tiles, make a parent tile by resampling
        |> ignore

        (remainingCommands, Some tile :: remainingTilesInStack)

    | Failure _ :: _ -> (commandStack, tilesStack)


let rec processCommandStack 
    localCacheDir
    determineTileStatus convertFromHgt readTilePngFile
    state
    : TileFetchingState =
    let updatedState = 
        processNextCommand
            localCacheDir
            determineTileStatus convertFromHgt readTilePngFile state

    match updatedState with
    // when there are no more commands to process, stop the tail recursion
    | ([], tiles) -> ([], tiles)
    // when there is an error indicator in the command stack, stop
    | (Failure _ :: _, _) -> updatedState
    // otherwise, process the next command in the stack
    | updatedState -> 
        processCommandStack 
            localCacheDir
            determineTileStatus convertFromHgt readTilePngFile
            updatedState

let initializeProcessingState tile =
    ([], [])
    |>
    newTileToProcess tile

/// <summary>
/// After the fetch tile processing stack has finished, this function checks
/// the final state and based on it, decides whether to load a PNG tile,
/// returns None (if the tile does not exist), or return an error (if the 
/// final state contains error information).
/// </summary>
let finalizeFetchSrtmTileProcessing 
    (readPngTile: SrtmTileCoords -> Result<HeightsArray, string>) 
    (finalState: TileFetchingState) =
    match finalState with
    | ([], [ Some tile ]) -> 
        readPngTile tile
        |> Result.map (fun heightsArray -> Some heightsArray)
    | ([], [ None ]) -> Ok None
    | (Failure message :: _, _) -> Error message
    | _ -> 
        invalidOp "bug: the command stack is neither empty nor it indicates an error"
