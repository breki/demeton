module Demeton.Srtm.Fetch

open Types
open Funcs

type LocalCacheTileStatus =
    | NotCached
    | HigherLevelDoesNotExist
    | Cached

type LocalCacheTileStatusChecker = SrtmTileCoords -> LocalCacheTileStatus

let checkLocalCacheTileStatus
    (localCacheDir: string) (fileExists: FileSys.FileExistsChecker)
    : LocalCacheTileStatusChecker =
    fun tile -> 

    let tileFileName = toLocalCacheTileFileName localCacheDir tile
    match fileExists tileFileName with
    | true -> Cached
    | false -> 
        match tile.Level.Value with
        | 0 -> NotCached    
        | _ -> 
            let notExistsIndicatorFileName = 
                tileFileName |> Pth.extension ".none"
            match fileExists notExistsIndicatorFileName with
            | true -> HigherLevelDoesNotExist
            | false -> NotCached

type SrtmDirTileStatus =
    | DoesNotExist
    | Exists

type SrtmDirTileStatusChecker = 
    SrtmLongitude -> SrtmLatitude -> SrtmDirTileStatus

let checkSrtmDirTileStatus srtmDir (fileExists: FileSys.FileExistsChecker)
    : SrtmDirTileStatusChecker = 
    fun lon lat ->

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

type SrtmTileStatusChecker = 
    SrtmTileCoords -> LocalCacheTileStatus -> SrtmTileStatus

let checkSrtmTileStatus 
    (checkSrtmDirTileStatus: SrtmDirTileStatusChecker): SrtmTileStatusChecker =
    fun tileCoords localCacheTileStatus ->

    match (tileCoords.Level.Value, localCacheTileStatus) with
    | (_, LocalCacheTileStatus.Cached) -> Cached
    | (0, LocalCacheTileStatus.NotCached) ->
        match checkSrtmDirTileStatus tileCoords.Lon tileCoords.Lat with
        | SrtmDirTileStatus.DoesNotExist -> NotExists
        | SrtmDirTileStatus.Exists -> NotCached
    | (level, LocalCacheTileStatus.NotCached) when level > 0 -> NotCached
    | (level, LocalCacheTileStatus.HigherLevelDoesNotExist) when level > 0 -> 
        NotExists
    | _ -> invalidOp "bug: this case should not never happen"
