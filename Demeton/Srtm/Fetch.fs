module Demeton.Srtm.Fetch

open Demeton.DemTypes
open Types
open Funcs
open Png
open Downsampling
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
    srtmTileCoords =

    srtmTileCoords
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

type CreateFromLowerTiles = { Parent: SrtmTileId; Children: SrtmTileId[] }

type TileProcessingCommand =
    | DetermineStatus of SrtmTileId
    | ConvertTileFromHgt of SrtmTileId 
    | CreateFromLowerTiles of CreateFromLowerTiles
    | Failure of string

type TileProcessingCommandStack = TileProcessingCommand list

type TileInStack = SrtmTile option
type TilesStack = TileInStack list

type TileFetchingState = (TileProcessingCommandStack * TilesStack)

let newTileToProcess tile ((stack, tilesBuffer): TileFetchingState):
    TileFetchingState =
    ((DetermineStatus tile) :: stack, tilesBuffer)

let determineTileStatus
    srtmDir
    localCacheDir
    (fileExists: FileSys.FileExistsChecker)
    (tile: SrtmTileId) = 

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
        checkSrtmDirTileStatus 
            srtmDir fileExists (tile |> toSrtmTileCoords)

    let localCacheStatus =
        determineLocalCacheTileStatus 
            tile.Level
            (tilePngExistsInLocalCache tile)
            (Lazy<bool>(fun () -> tileNoneFileExistsInLocalCache tile))

    decideSrtmTileStatus
        tile.Level
        localCacheStatus
        (Lazy<SrtmDirTileStatus>(fun () -> checkSrtmDirTileStatus()))

let fetchFirstNOfTiles tilesCount = List.splitAt tilesCount

let processNextCommand 
    (localCacheDir: FileSys.DirectoryName)
    (srtmDir: FileSys.DirectoryName)
    determineTileStatus
    (readPngTile: SrtmPngTileReader)
    (convertFromHgt: SrtmHgtToPngTileConverter)
    (constructHigherLevelTile: HigherLevelTileConstructor)
    (writeTileToCache: SrtmTileCacheWriter)
    ((commandStack, tilesStack): TileFetchingState)
    : TileFetchingState =
    match commandStack with
    | [] -> ([], tilesStack)

    | DetermineStatus tile :: remainingCommands -> 
        match determineTileStatus tile with
        | NotExists -> (remainingCommands, None :: tilesStack)
        | Cached ->
            // read the tile heights array from the cache
            // and then put it to the stack
            let heightsArrayResult =
                tile 
                |> toLocalCacheTileFileName localCacheDir
                |> readPngTile tile
            match heightsArrayResult with
            | Ok heightsArray ->
                (remainingCommands, Some (tile, heightsArray) :: tilesStack)
            | Error message ->
                (Failure message :: remainingCommands, tilesStack)
        | NotCached when tile.Level.Value = 0 -> 
            ((ConvertTileFromHgt tile) :: remainingCommands, tilesStack)
        | NotCached ->
            let childrenTiles = 
                tile 
                |> childrenTilesNeededForDownsampling 
                    DownsamplingMethod.Average
            let cmd = { Parent = tile; Children = childrenTiles }
            let childrenCommands = 
                childrenTiles |> Array.map DetermineStatus |> Array.toList
            let updatedStack = 
                List.append 
                    childrenCommands 
                    ((CreateFromLowerTiles cmd) :: remainingCommands)
            (updatedStack, tilesStack)

    | ConvertTileFromHgt tile :: remainingCommands ->
        let zippedHgtFileName = 
            tile 
            |> toSrtmTileCoords
            |> toZippedSrtmTileFileName srtmDir
        let pngFileName = tile |> toLocalCacheTileFileName localCacheDir

        match convertFromHgt tile zippedHgtFileName pngFileName with
        | Ok heightsArray ->
            (remainingCommands, Some (tile, heightsArray) :: tilesStack)
        | Error message -> (Failure message :: remainingCommands, tilesStack)

    | CreateFromLowerTiles parameters :: remainingCommands ->
        let tile = parameters.Parent
       
        // fetch the required number of tiles from the tiles stack
        let lowerTilesNumber = parameters.Children.Length
        let (lowerTiles, remainingTilesInStack) = 
            tilesStack |> fetchFirstNOfTiles lowerTilesNumber

        let childrenTiles =
            lowerTiles
            |> List.filter Option.isSome
            |> List.map Option.get

        let higherLevelHeightsArray =
            constructHigherLevelTile
                DownsamplingMethod.Average tile childrenTiles
            |> writeTileToCache tile
            
        match higherLevelHeightsArray with
        |Some tile ->
            (remainingCommands, Some tile :: remainingTilesInStack)
        // when the constructed tile is empty
        | None -> (remainingCommands, None :: remainingTilesInStack)

    | Failure _ :: _ -> (commandStack, tilesStack)


let rec processCommandStack 
    (localCacheDir: FileSys.DirectoryName)
    (srtmDir: FileSys.DirectoryName)
    determineTileStatus
    (readPngTile: SrtmPngTileReader)
    convertFromHgt
    (constructHigherLevelTile: HigherLevelTileConstructor)
    (writeTileToCache: SrtmTileCacheWriter)
    state
    : TileFetchingState =

    let updatedState = 
        processNextCommand
            localCacheDir srtmDir
            determineTileStatus
            readPngTile
            convertFromHgt 
            constructHigherLevelTile
            writeTileToCache
            state

    match updatedState with
    // when there are no more commands to process, stop the tail recursion
    | ([], tiles) -> ([], tiles)
    // when there is an error indicator in the command stack, stop
    | (Failure _ :: _, _) -> updatedState
    // otherwise, process the next command in the stack
    | updatedState -> 
        processCommandStack 
            localCacheDir srtmDir
            determineTileStatus
            readPngTile
            convertFromHgt 
            constructHigherLevelTile
            writeTileToCache
            updatedState

let initializeProcessingState tile =
    ([], [])
    |>
    newTileToProcess tile

/// <summary>
/// After the fetch tile processing stack has finished, this function checks
/// the final state and based on it, decides whether to return a tile heights
/// array, return None (if the tile does not exist), or return an error (if the 
/// final state contains error information).
/// </summary>
let finalizeFetchSrtmTileProcessing 
    (finalState: TileFetchingState) =
    match finalState with
    | ([], [ Some (_, heightsArray) ]) -> Ok (Some heightsArray)
    | ([], [ None ]) -> Ok None
    | (Failure message :: _, _) -> Error message
    | _ -> 
        invalidOp
            "bug: the command stack is neither empty nor it indicates an error"
