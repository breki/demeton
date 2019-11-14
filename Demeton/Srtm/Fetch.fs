module Demeton.Srtm.Fetch

open Types
open Funcs

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

type TileProcessingCommandStack = TileProcessingCommand list

type TileInStack = SrtmTileCoords option
type TilesStack = TileInStack list

type TileFetchingState = (TileProcessingCommandStack * TilesStack)

let newTileToProcess tile ((stack, tilesBuffer): TileFetchingState):
    TileFetchingState =
    ((DetermineStatus tile) :: stack, tilesBuffer)

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

let fetchFirstNOfTiles tilesCount = List.splitAt tilesCount

let processNextCommand 
    determineTileStatus
    convertFromHgt
    createFromLowerTiles
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
        convertFromHgt tile |> ignore
        (remainingCommands, Some tile :: tilesStack)

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

        createFromLowerTiles tile lowerTilesWithoutNone |> ignore
        (remainingCommands, Some tile :: remainingTilesInStack)


let rec processCommandStack 
    determineTileStatus convertFromHgt createFromLowerTiles
    state
    : TileFetchingState =
    match processNextCommand
        determineTileStatus convertFromHgt createFromLowerTiles
        state with
    | ([], tiles) -> ([], tiles)
    | updatedState -> 
        processCommandStack 
            determineTileStatus convertFromHgt createFromLowerTiles
            updatedState
