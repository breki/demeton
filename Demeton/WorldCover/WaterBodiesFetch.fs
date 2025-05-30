module Demeton.WorldCover.WaterBodiesFetch

open System
open System.IO
open LibExt
open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Demeton.WorldCover.Types
open Demeton.WorldCover.Funcs
open Demeton.WorldCover.Fetch
open Demeton.WaterBodies.Types
open Demeton.WaterBodies.Png
open Raster
open FileSys

/// <summary>
/// Unpacks a WorldCover TIFF tile into a 3x3 grid of water bodies tiles.
/// </summary>
/// <param name="worldCoverTileId">
/// The identifier of the WorldCover tile to be unpacked.
/// </param>
/// <param name="heightsArray">
/// The heights array representing the raster data of the WorldCover tile.
/// </param>
/// <returns>
/// A 3x3 array where each element is a tuple containing:
/// - The tile ID of the water bodies tile.
/// - The extracted heights array for the water bodies tile.
/// </returns>
/// <remarks>
/// This function converts the original WorldCover raster data into a
/// monochrome raster where water is represented by 1 and everything else by 0.
/// It then divides the raster into 3x3 subtiles, each corresponding to a
/// water bodies tile, and extracts the relevant data for each subtile.
/// </remarks>
let unpackWaterBodiesTilesFromWorldCoverTile
    worldCoverTileId
    (heightsArray: HeightsArray)
    =
    Log.debug
        "Unpacking WorldCover TIFF tile %s into 3x3 water bodies tiles..."
        (toTileName worldCoverTileId)

    let waterBodiesHeightsArray3by3 =
        heightsArray |> convertWorldCoverRasterToWaterMonochrome

    // make a 3x3 array of water bodies heights arrays made from chunks of the
    // original 3x3 heights array
    Array2D.init 3 3 (fun x y ->
        let tileId =
            demTileXYId
                (worldCoverTileId.TileX + x)
                (worldCoverTileId.TileY + y)

        Log.debug "Extracting water bodies tile %s..." (toTileName tileId)

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
            Log.debug
                "Loading water bodies tile %s from '%s'..."
                (toTileName tileId)
                cachedPngFileName

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


/// <summary>
/// Extracts a water bodies tile from a WorldCover tile if needed.
/// </summary>
/// <param name="cacheDir">
/// The directory where cached files are stored.
/// </param>
/// <param name="cachedLoadResult">
/// The result of attempting to load the water bodies tile from the cache.
/// </param>
/// <returns>
/// A `CachedWaterBodiesTileLoadResult` indicating the result of the extraction:
/// - If the tile needs to be downloaded, it downloads the WorldCover tile,
///   extracts the water bodies tile, and caches it.
/// - If the tile does not exist in the WorldCover data, it creates a `.none`
///   file in the cache.
/// - If the tile is already cached, it returns the cached result.
/// </returns>
/// <remarks>
/// This function ensures that the water bodies tile is available in the cache,
/// either by loading it from an existing cache or by extracting it from the
/// corresponding WorldCover tile.
/// </remarks>
let extractWaterBodiesTileFromWorldCoverTileIfNeeded cacheDir cachedLoadResult =
    match cachedLoadResult with
    | TileNeedsToBeDownloaded(waterBodiesTileId, containingWorldCoverTileId) ->
        let _tiffFileName =
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

            Log.debug
                "Writing water bodies tile %s to '%s'..."
                (toTileName tileId)
                cachedPngFileName

            match openFileToWrite cachedPngFileName with
            | Ok stream ->
                stream
                |> encodeWaterBodiesHeightsArrayToPng heightsArray
                |> flushStream
                |> closeStream
            | Error error -> raise error.Exception)

        // try to find the corresponding water bodies tile
        let waterBodiesTileMaybe =
            tilesArray
            |> Array2DExt.toSeq
            |> Seq.tryFind (fun (tileId, _) -> tileId = waterBodiesTileId)

        // note that it is not guaranteed the water bodies tile actually
        // exists - the area could be over an ocean
        match waterBodiesTileMaybe with
        | Some(_, waterBodiesTile) ->
            waterBodiesTile |> Some |> CachedTileLoaded
        | None -> TileDoesNotExistInWorldCover waterBodiesTileId

    | result -> result
