module Demeton.WorldCover.WaterBodiesFetch

open System
open System.IO
open LibExt
open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Demeton.WorldCover.Types
open Demeton.WorldCover.Funcs
open Demeton.WorldCover.Fetch
open Demeton.WaterBodies.Png
open Raster
open FileSys

let unpackWaterBodiesTilesFromWorldCoverTile
    worldCoverTileId
    (heightsArray: HeightsArray)
    =
    let waterBodiesHeightsArray3by3 =
        heightsArray |> convertWorldCoverRasterToWaterMonochrome

    // make a 3x3 array of water bodies heights arrays made from chunks of the
    // original 3x3 heights array
    Array2D.init 3 3 (fun x y ->
        let tileId =
            demTileXYId
                (worldCoverTileId.TileX + x)
                (worldCoverTileId.TileY + y)

        let minX, minY = tileMinCell WorldCoverTileSize tileId

        let waterBodiesTileRect: Rect =
            { MinX = minX
              MinY = minY
              Width = WorldCoverTileSize
              Height = WorldCoverTileSize }

        (tileId, extract waterBodiesTileRect waterBodiesHeightsArray3by3))

type WaterBodiesTileCacheStatus =
    | CachedPng of (DemTileId * FileName)
    | CachedNoneFile of DemTileId
    | NotCached of DemTileId

let determineWaterBodiesTileCacheStatus
    waterBodiesPngFileName
    waterBodiesNoneFileName
    (tileId: DemTileId)
    : WaterBodiesTileCacheStatus =

    match waterBodiesPngFileName, waterBodiesNoneFileName with
    | Some waterBodiesPngFileName, _ ->
        CachedPng(tileId, waterBodiesPngFileName)
    | _, Some waterBodiesNoneFileName -> CachedNoneFile(tileId)
    | None, None -> NotCached(tileId)

type CachedWaterBodiesTileLoadResult =
    | CachedTileLoaded of HeightsArray option
    | TileNeedsToBeDownloaded of DemTileId * DemTileId
    | TileDoesNotExistInWorldCover of DemTileId

let cachedWaterBodiesPngFileName cacheDir tileId =
    Path.Combine(
        cacheDir,
        WaterBodiesCacheSubdirName,
        $"WaterBodies-%s{toTileName tileId}.png"
    )

let cachedWaterBodiesNoneFileName cacheDir tileId =
    Path.Combine(
        cacheDir,
        WaterBodiesCacheSubdirName,
        $"WaterBodies-%s{toTileName tileId}.none"
    )

let loadWaterBodiesTileFromCache
    cacheDir
    (availableWorldCoverTiles: Set<DemTileId>)
    tileId
    : CachedWaterBodiesTileLoadResult =
    let cachedPngFileName = cachedWaterBodiesPngFileName cacheDir tileId
    let cachedNoneFileName = cachedWaterBodiesNoneFileName cacheDir tileId

    determineWaterBodiesTileCacheStatus
        (if fileExists cachedPngFileName then
             Some cachedPngFileName
         else
             None)
        (if fileExists cachedNoneFileName then
             Some cachedNoneFileName
         else
             None)
        tileId
    |> function
        | CachedNoneFile _ -> CachedTileLoaded None
        | CachedPng(tileId, cachedPngFileName) ->
            cachedPngFileName
            |> decodeWaterBodiesTileFromPngFile WorldCoverTileSize tileId
            |> function
                | Ok heightsArray -> CachedTileLoaded(Some heightsArray)
                | Error error -> raise (InvalidOperationException error)
        | NotCached _ ->
            let containingWorldCoverTileId =
                containingWorldCoverFileTileId tileId

            if
                (availableWorldCoverTiles
                 |> Set.contains containingWorldCoverTileId)
            then
                TileNeedsToBeDownloaded(tileId, containingWorldCoverTileId)
            else
                TileDoesNotExistInWorldCover tileId


let makeNoneFileIfNeeded cacheDir cachedLoadResult =
    match cachedLoadResult with
    | TileDoesNotExistInWorldCover tileId ->
        let noneFileName = cachedWaterBodiesNoneFileName cacheDir tileId

        match openFileToWrite noneFileName with
        | Ok stream ->
            stream |> closeStream
            CachedTileLoaded None
        | Error error -> raise error.Exception
    | result -> result


let extractWaterBodiesTileFromWorldCoverTileIfNeeded cacheDir cachedLoadResult =
    match cachedLoadResult with
    | TileNeedsToBeDownloaded(waterBodiesTileId, containingWorldCoverTileId) ->
        let tiffFileName =
            ensureWorldCoverFile
                cacheDir
                fileExists
                downloadFile
                containingWorldCoverTileId

        let worldCoverHeightsArray =
            readWorldCoverTiffFile cacheDir None containingWorldCoverTileId

        let tilesArray =
            worldCoverHeightsArray
            |> unpackWaterBodiesTilesFromWorldCoverTile
                containingWorldCoverTileId

        Path.Combine(cacheDir, WaterBodiesCacheSubdirName)
        |> ensureDirectoryExists
        |> Result.mapError (fun error -> raise error.Exception)
        |> ignore

        tilesArray
        |> Array2D.iter (fun (tileId, heightsArray) ->
            let cachedPngFileName =
                cachedWaterBodiesPngFileName cacheDir tileId

            match openFileToWrite cachedPngFileName with
            | Ok stream ->
                stream
                |> encodeWaterBodiesHeightsArrayToPng heightsArray
                |> ignore
            | Error error -> raise error.Exception)

        // use the correct water bodies tile from the array and return it
        tilesArray
        |> Array2DExt.toSeq
        |> Seq.filter (fun (tileId, _) -> tileId = waterBodiesTileId)
        |> Seq.head
        |> snd
        |> Some
        |> CachedTileLoaded

    | result -> result
