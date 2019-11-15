module Demeton.Srtm.Fetch

open Types
open Funcs
open Png
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

let readPngTilesBatch 
    localCacheDir 
    (readPngTile: SrtmPngTileReader) 
    (tiles: SrtmTileCoords list)
    : Result<HeightsArray list, string> =
     
    let readPngTile readingState tile =
        match readingState with
        | Ok heightsArrays ->
            tile 
            |> toLocalCacheTileFileName localCacheDir
            |> readPngTile tile
            |> Result.map (fun heightsArray -> heightsArray :: heightsArrays)
        | Error message -> Error message

    tiles |> List.fold readPngTile (Ok [])


let downsampleHeightPoint (source: HeightsArray) x y = 
    let childX = x <<< 1
    let childY = y <<< 1

    source.heightAt (childX, childY)

let downsampleTileHeightsArray 
    tileSize tile (heightsArrayMaybe: HeightsArray option)
    : HeightsArray option =
    let (tileMinX, tileMinY)
        = tile |> Tile.tileCellMinCoords tileSize

    heightsArrayMaybe
    |> Option.map (fun heightsArray -> 
        HeightsArray(tileMinX, tileMinY, tileSize, tileSize, 
            HeightsArrayInitializer2D (fun (x, y) -> 
                downsampleHeightPoint heightsArray x y)))

/// <summary>
/// Constructs a heights array for a higher-level tile from the list of 
/// children lower-level tiles.
/// </summary>
type HigherLevelTileConstructor = 
    SrtmTileCoords -> SrtmTileCoords list -> Result<HeightsArray option, string>

/// <summary>
/// Constructs a heights array for a higher-level tile from the list of 
/// children lower-level tiles.
/// </summary>
let constructHigherLevelTileHeightsArray 
    (tileSize: int)
    (localCacheDir: FileSys.DirectoryName)
    (readTilePngFile: SrtmPngTileReader): HigherLevelTileConstructor =
    fun (tile: SrtmTileCoords) (childrenTiles: SrtmTileCoords list) ->

    readPngTilesBatch localCacheDir readTilePngFile childrenTiles
    |> Result.map (fun heightsArrays -> 
        Log.info "Merging all the read tiles into a single array..."
        heightsArrays |> Demeton.Dem.merge)
    // after merging the tiles, make a parent tile by downsampling
    |> Result.map (fun heightsArray -> 
        Log.info "Creating the higher-level tile by downsampling..."
        heightsArray 
        |> downsampleTileHeightsArray tileSize tile)

let fetchFirstNOfTiles tilesCount = List.splitAt tilesCount

let processNextCommand 
    (localCacheDir: FileSys.DirectoryName)
    (srtmDir: FileSys.DirectoryName)
    determineTileStatus
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
        let zippedHgtFileName = tile |> toZippedSrtmTileFileName srtmDir
        let pngFileName = tile |> toLocalCacheTileFileName localCacheDir

        match convertFromHgt tile zippedHgtFileName pngFileName with
        | Ok _ -> (remainingCommands, Some tile :: tilesStack)
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

        constructHigherLevelTile tile childrenTiles
        |> Result.map (writeTileToCache tile)
        // todo: handle errors
        |> ignore

        (remainingCommands, Some tile :: remainingTilesInStack)

    | Failure _ :: _ -> (commandStack, tilesStack)


let rec processCommandStack 
    (localCacheDir: FileSys.DirectoryName)
    (srtmDir: FileSys.DirectoryName)
    determineTileStatus convertFromHgt 
    (constructHigherLevelTile: HigherLevelTileConstructor)
    (writeTileToCache: SrtmTileCacheWriter)
    state
    : TileFetchingState =

    let updatedState = 
        processNextCommand
            localCacheDir srtmDir
            determineTileStatus 
            convertFromHgt 
            constructHigherLevelTile writeTileToCache
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
            convertFromHgt 
            constructHigherLevelTile writeTileToCache
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
    localCacheDir
    (readPngTile: SrtmPngTileReader) 
    (finalState: TileFetchingState) =
    match finalState with
    | ([], [ Some tile ]) -> 
        tile |> toLocalCacheTileFileName localCacheDir
        |> readPngTile tile
        |> Result.map (fun heightsArray -> Some heightsArray)
    | ([], [ None ]) -> Ok None
    | (Failure message :: _, _) -> Error message
    | _ -> 
        invalidOp "bug: the command stack is neither empty nor it indicates an error"
