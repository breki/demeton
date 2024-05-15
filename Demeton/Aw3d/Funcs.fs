module Demeton.Aw3d.Funcs

open System.IO
open Demeton.Aw3d.Types
open Demeton.Geometry.Common
open Demeton.Srtm.Funcs
open FileSys

/// <summary>
/// Given a bounding box, returns a sequence of AW3D tiles that cover it.
/// </summary>
let boundsToAw3dTiles (bounds: LonLatBounds) : Aw3dTileId seq =
    let cellsPerDegree = 3600
    let tileSize = 3600

    let minTileX =
        bounds.MinLon
        |> longitudeToCellX cellsPerDegree
        |> cellXToTileX tileSize
        |> floor
        |> int

    let minTileY =
        bounds.MaxLat
        |> latitudeToCellY cellsPerDegree
        |> cellYToTileY tileSize
        |> floor
        |> int

    let maxTileX =
        (bounds.MaxLon
         |> longitudeToCellX cellsPerDegree
         |> cellXToTileX tileSize
         |> ceil
         |> int)
        - 1

    let maxTileY =
        (bounds.MinLat
         |> latitudeToCellY cellsPerDegree
         |> cellYToTileY tileSize
         |> ceil
         |> int)
        - 1

    seq {
        for tileY in [ minTileY..maxTileY ] do
            for tileX in [ minTileX..maxTileX ] do
                yield { TileX = tileX; TileY = tileY }
    }

/// <summary>
/// Returns the download URL for the given AW3D tile.
/// </summary>
let aw3dTileDownloadUrl (tileId: Aw3dTileId) : string =
    let groupTileX = tileId.TileX / 5 * 5
    let groupTileY = tileId.TileY / 5 * 5

    let groupTileId =
        { TileX = groupTileX
          TileY = groupTileY }

    sprintf
        "https://www.eorc.jaxa.jp/ALOS/aw3d30/data/release_v2303/%s/%s.zip"
        groupTileId.Aw3dTileName
        tileId.Aw3dTileName


/// <summary>
/// Returns the path to the cached ZIP file for the given AW3D tile.
/// </summary>
let aw3dTileCachedZipFileName cacheDir (tileId: Aw3dTileId) =
    Path.Combine(cacheDir, Aw3dDirName, $"{tileId.Aw3dTileName}.zip")

/// <summary>
/// Returns the path to the cached TIFF file for the given AW3D tile.
/// </summary>
let aw3dTileCachedTifFileName cacheDir (tileId: Aw3dTileId) =
    Path.Combine(cacheDir, Aw3dDirName, $"{tileId.Aw3dTileName}.tif")

/// <summary>
/// Returns the name of the TIFF file entry in the AW3D tile ZIP file.
/// </summary>
let aw3dTileZipFileEntryName (tileId: Aw3dTileId) =
    $"{tileId.Aw3dTileName}/ALPSMLC30_{tileId.Aw3dTileName}_DSM.tif"

/// <summary>
/// Ensures the specified AW3D tile TIFF file is available in the cache
/// directory, downloading it if necessary.
/// </summary>
let ensureAw3dTile
    cacheDir
    fileExists
    downloadFile
    (readZipFile: ZipFileReader<string>)
    copyStreamToFile
    (deleteFile: string -> Result<string, FileSysError>)
    tileId
    =
    let downloadTileZipFile tileId =
        let url = aw3dTileDownloadUrl tileId
        let cachedTileZipFileName = tileId |> aw3dTileCachedZipFileName cacheDir

        Log.debug
            $"Downloading AW3D tile %s{tileId.Aw3dTileName} from %s{url}..."

        downloadFile url cachedTileZipFileName

    let cachedTifFileName = aw3dTileCachedTifFileName cacheDir tileId

    if fileExists cachedTifFileName then
        Ok cachedTifFileName
    else
        // download the tile
        let cachedTileZipFileName = tileId |> downloadTileZipFile

        let tiffFileNameInZip = aw3dTileZipFileEntryName tileId

        let extractTiffFileFromZip tiffFileStream =
            let tiffFilePath = tileId |> aw3dTileCachedTifFileName cacheDir
            tiffFileStream |> copyStreamToFile tiffFilePath

        match
            readZipFile
                cachedTileZipFileName
                tiffFileNameInZip
                extractTiffFileFromZip
        with
        | Ok tiffFilePath ->
            match deleteFile cachedTileZipFileName with
            | Ok _ -> Ok tiffFilePath
            | Error error -> Error error.Exception.Message
        | Error error -> Error error.Exception.Message
