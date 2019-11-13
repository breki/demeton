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
