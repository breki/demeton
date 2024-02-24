module Demeton.Srtm.Fetch

open Types
open Funcs
open Png
open Downsampling
open FileSys
open System

type LocalCacheTileStatus =
    | NotCached
    | HigherLevelDoesNotExist
    | Cached

/// <summary>
/// Used to determine the status of a tile in the local cache.
/// </summary>
/// <param name="level">The level of the SRTM tile.</param>
/// <param name="tilePngExistsInLocalCache">
/// Indicates whether the tile PNG file exists in the local cache.
/// </param>
/// <param name="tileNoneFileExistsInLocalCache">
/// A lazy value indicating whether the "tile none" file exists in the local
/// cache.
/// </param>
let determineLocalCacheTileStatus
    (level: SrtmLevel)
    tilePngExistsInLocalCache
    (tileNoneFileExistsInLocalCache: Lazy<bool>)
    =
    match tilePngExistsInLocalCache with
    | true -> Cached
    | false ->
        match level with
        | Level0 -> NotCached
        | HigherLevel ->
            match tileNoneFileExistsInLocalCache.Value with
            | true -> HigherLevelDoesNotExist
            | false -> NotCached

type SrtmDirTileStatus =
    | DoesNotExist
    | Exists

let checkSrtmDirTileStatus
    srtmDir
    (fileExists: FileExistsChecker)
    srtmTileCoords
    =

    srtmTileCoords
    |> toZippedSrtmTileFileName srtmDir
    |> fileExists
    |> function
        | true -> Exists
        | false -> DoesNotExist


/// <summary>
/// Defines the caching status of an SRTM tile.
/// </summary>
type SrtmTileStatus =
    | NotExists
    | NotCached
    | Cached


/// <summary>
/// Used to determine the caching status of an SRTM tile based on its level and
/// local cache status.
/// </summary>
/// <param name="level">The level of the SRTM tile.</param>
/// <param name="localCacheTileStatus">
/// Status of the tile in the local cache.</param>
/// <param name="srtmTileStatus">
/// Status of the tile in the SRTM directory. This value is lazily evaluated.
/// </param>
let decideSrtmTileStatus
    (level: SrtmLevel)
    localCacheTileStatus
    (srtmTileStatus: Lazy<SrtmDirTileStatus>)
    =

    match (level.Value, localCacheTileStatus) with
    | _, LocalCacheTileStatus.Cached -> Cached
    | 0, LocalCacheTileStatus.NotCached ->
        match srtmTileStatus.Value with
        | SrtmDirTileStatus.DoesNotExist -> NotExists
        | SrtmDirTileStatus.Exists -> NotCached
    | level, LocalCacheTileStatus.NotCached when level > 0 -> NotCached
    | level, LocalCacheTileStatus.HigherLevelDoesNotExist when level > 0 ->
        NotExists
    | _ -> invalidOp "bug: this should never happen"

type CreateFromLowerTiles =
    { Parent: SrtmTileId
      Children: SrtmTileId[] }

type TileProcessingCommand =
    | DetermineStatus of SrtmTileId
    | ConvertTileFromHgt of SrtmTileId
    | CreateFromLowerTiles of CreateFromLowerTiles
    | Failure of string

type TileProcessingCommandStack = TileProcessingCommand list

type TileInStack = SrtmTile option
type TilesStack = TileInStack list

type TileFetchingState = TileProcessingCommandStack * TilesStack

let newTileToProcess
    tile
    ((stack, tilesBuffer): TileFetchingState)
    : TileFetchingState =
    ((DetermineStatus tile) :: stack, tilesBuffer)


/// <summary>
/// Used to determine the caching status of an SRTM tile.
/// </summary>
/// <param name="srtmDir">The directory where the SRTM data is stored.</param>
/// <param name="localCacheDir">The directory where the local cache is stored.
/// </param>
/// <param name="fileExists">A function that checks if a file exists.</param>
/// <param name="tile">The SRTM tile to determine the status for.</param>
let determineTileStatus
    srtmDir
    localCacheDir
    (fileExists: FileExistsChecker)
    (tile: SrtmTileId)
    =

    let tilePngExistsInLocalCache tile =
        tile |> toLocalCacheTileFileName localCacheDir |> fileExists

    /// <summary>
    /// The `tileNoneFileExistsInLocalCache` function checks if the "tile none"
    /// file exists in the local cache.
    /// </summary>
    /// <param name="tile">The SRTM tile to check for.</param>
    let tileNoneFileExistsInLocalCache tile =
        tile
        |> toLocalCacheTileFileName localCacheDir
        |> Pth.extension ".none"
        |> fileExists

    let checkSrtmDirTileStatus () =
        checkSrtmDirTileStatus srtmDir fileExists (tile |> toSrtmTileCoords)

    let localCacheStatus =
        determineLocalCacheTileStatus
            tile.Level
            (tilePngExistsInLocalCache tile)
            (Lazy<bool>(fun () -> tileNoneFileExistsInLocalCache tile))

    decideSrtmTileStatus
        tile.Level
        localCacheStatus
        (Lazy<SrtmDirTileStatus>(fun () -> checkSrtmDirTileStatus ()))

let fetchFirstNOfTiles tilesCount = List.splitAt tilesCount

let processNextCommand
    (localCacheDir: DirectoryName)
    (srtmDir: DirectoryName)
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
                (remainingCommands, Some(tile, heightsArray) :: tilesStack)
            | Error message ->
                (Failure message :: remainingCommands, tilesStack)
        | NotCached when tile.Level.Value = 0 ->
            ((ConvertTileFromHgt tile) :: remainingCommands, tilesStack)
        | NotCached ->
            let childrenTiles =
                tile
                |> childrenTilesNeededForDownsampling DownsamplingMethod.Average

            let cmd =
                { Parent = tile
                  Children = childrenTiles }

            let childrenCommands =
                childrenTiles |> Array.map DetermineStatus |> Array.toList

            let updatedStack =
                List.append
                    childrenCommands
                    ((CreateFromLowerTiles cmd) :: remainingCommands)

            (updatedStack, tilesStack)

    | ConvertTileFromHgt tile :: remainingCommands ->
        let zippedHgtFileName =
            tile |> toSrtmTileCoords |> toZippedSrtmTileFileName srtmDir

        let pngFileName = tile |> toLocalCacheTileFileName localCacheDir

        match convertFromHgt tile zippedHgtFileName pngFileName with
        | Ok heightsArray ->
            (remainingCommands, Some(tile, heightsArray) :: tilesStack)
        | Error message -> (Failure message :: remainingCommands, tilesStack)

    | CreateFromLowerTiles parameters :: remainingCommands ->
        let tile = parameters.Parent

        // fetch the required number of tiles from the tiles stack
        let lowerTilesNumber = parameters.Children.Length

        let lowerTiles, remainingTilesInStack =
            tilesStack |> fetchFirstNOfTiles lowerTilesNumber

        let childrenTiles =
            lowerTiles |> List.filter Option.isSome |> List.map Option.get

        let higherLevelHeightsArrayResult =
            constructHigherLevelTile
                DownsamplingMethod.Average
                tile
                childrenTiles
            |> writeTileToCache tile

        match higherLevelHeightsArrayResult with
        | Ok(Some tile) ->
            (remainingCommands, Some tile :: remainingTilesInStack)
        // when the constructed tile is empty
        | Ok None -> (remainingCommands, None :: remainingTilesInStack)
        | Error error ->
            (Failure(error |> fileSysErrorMessage) :: remainingCommands,
             tilesStack)

    | Failure _ :: _ -> (commandStack, tilesStack)


let rec processCommandStack
    (localCacheDir: DirectoryName)
    (srtmDir: DirectoryName)
    determineTileStatus
    (readPngTile: SrtmPngTileReader)
    convertFromHgt
    (constructHigherLevelTile: HigherLevelTileConstructor)
    (writeTileToCache: SrtmTileCacheWriter)
    state
    : TileFetchingState =

    let updatedState =
        processNextCommand
            localCacheDir
            srtmDir
            determineTileStatus
            readPngTile
            convertFromHgt
            constructHigherLevelTile
            writeTileToCache
            state

    match updatedState with
    // when there are no more commands to process, stop the tail recursion
    | [], tiles -> ([], tiles)
    // when there is an error indicator in the command stack, stop
    | Failure _ :: _, _ -> updatedState
    // otherwise, process the next command in the stack
    | updatedState ->
        processCommandStack
            localCacheDir
            srtmDir
            determineTileStatus
            readPngTile
            convertFromHgt
            constructHigherLevelTile
            writeTileToCache
            updatedState

let initializeProcessingState tile = ([], []) |> newTileToProcess tile

/// <summary>
/// After the fetch tile processing stack has finished, this function checks
/// the final state and based on it, decides whether to return a tile heights
/// array, return None (if the tile does not exist), or return an error (if the
/// final state contains error information).
/// </summary>
let finalizeFetchSrtmTileProcessing (finalState: TileFetchingState) =
    match finalState with
    | [], [ Some(_, heightsArray) ] -> Ok(Some heightsArray)
    | [], [ None ] -> Ok None
    | Failure message :: _, _ -> Error message
    | _ ->
        invalidOp
            "bug: the command stack is neither empty nor it indicates an error"
