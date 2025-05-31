module Demeton.Aw3d.Funcs

open System
open System.IO
open BitMiracle.LibTiff.Classic
open Demeton.Aw3d.Types
open Demeton.Dem.Types
open Demeton.Geometry.Common
open Demeton.Dem.Funcs
open FileSys


/// <summary>
/// Returns the download URL for the given AW3D tile.
/// </summary>
let aw3dTileDownloadUrl (tileId: DemTileId) : string =
    let groupTileX =
        match tileId.TileX with
        | x when x >= 0 -> x / 5 * 5
        | x -> (x - 4) / 5 * 5

    let groupTileY =
        match tileId.TileY with
        | y when y >= 0 -> y / 5 * 5
        | y -> (y - 4) / 5 * 5

    let groupTileId = demTileXYId groupTileX groupTileY

    sprintf
        "https://www.eorc.jaxa.jp/ALOS/aw3d30/data/release_v2404/%s/%s.zip"
        groupTileId.FormatLat3Lon3
        tileId.FormatLat3Lon3


/// <summary>
/// Returns the path to the cached ZIP file for the given AW3D tile.
/// </summary>
let aw3dTileCachedZipFileName cacheDir (tileId: DemTileId) : FileName =
    Path.Combine(cacheDir, Aw3dDirName, $"{tileId.FormatLat3Lon3}.zip")

/// <summary>
/// Returns the path to the cached TIFF file for the given AW3D tile.
/// </summary>
let aw3dTileCachedTifFileName cacheDir (tileId: DemTileId) : FileName =
    Path.Combine(cacheDir, Aw3dDirName, $"{tileId.FormatLat3Lon3}.tif")

/// <summary>
/// Returns the name of the TIFF file entry in the AW3D tile ZIP file.
/// </summary>
let aw3dTileZipFileEntryName (tileId: DemTileId) =
    $"{tileId.FormatLat3Lon3}/ALPSMLC30_{tileId.FormatLat3Lon3}_DSM.tif"

/// <summary>
/// Ensures the specified AW3D tile TIFF file is available in the cache
/// directory, downloading it if necessary.
/// </summary>
/// <remarks>
/// If the function finds a ".none" file in the cache directory, it means the
/// tile is not available at the source, and the function returns Ok None.
/// </remarks>
let ensureAw3dTile
    cacheDir
    fileExists
    (downloadFile: FileName -> FileName -> FileName option)
    (readZipFile: ZipFileReader<string>)
    copyStreamToFile
    (deleteFile: FileName -> Result<FileName, FileSysError>)
    (openFileToWrite: FileWriter)
    tileId
    : Result<FileName option, string> =
    let downloadTileZipFile tileId =
        let url = aw3dTileDownloadUrl tileId
        let cachedTileZipFileName = tileId |> aw3dTileCachedZipFileName cacheDir

        Log.debug
            $"Downloading AW3D tile %s{tileId.FormatLat3Lon3} from %s{url}..."

        downloadFile url cachedTileZipFileName

    let cachedTifFileName = aw3dTileCachedTifFileName cacheDir tileId

    let cachedNoneFileName: FileName =
        Path.ChangeExtension(cachedTifFileName, ".none")

    if fileExists cachedTifFileName then
        cachedTifFileName |> Some |> Ok
    elif fileExists cachedNoneFileName then
        // if the ".none" file exists, it means the tile is not available
        // at the source
        None |> Ok
    else
        // download the tile
        let cachedTileZipFileName = tileId |> downloadTileZipFile

        match cachedTileZipFileName with
        | Some cachedTileZipFileName ->
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
                | Ok _ -> tiffFilePath |> Some |> Ok
                | Error error -> Error error.Exception.Message
            | Error error -> Error error.Exception.Message
        | None ->
            // While trying to download the AW3D tile, we determined it
            // does not actually exist on the server. We create a ".none"
            // file in the cache directory to indicate this so that we
            // don't try to download the tile again in the future.

            cachedNoneFileName
            |> Pth.directory
            |> ensureDirectoryExists
            |> ignore

            openFileToWrite cachedNoneFileName
            |> Result.map (fun stream ->
                stream |> closeStream
                None)
            |> ignore

            Ok None


let ensureAw3dTiles
    cacheDir
    (bounds: LonLatBounds)
    : Result<DemTileId list, string> =
    Log.info "Ensuring all needed AW3D tiles are there..."

    Log.info
        "Geo area needed: minLon: %f, minLat: %f, maxLon: %f, maxLat: %f"
        bounds.MinLon
        bounds.MinLat
        bounds.MaxLon
        bounds.MaxLat

    // todo 5: how to deal with half-width tiles here?
    let aw3dTilesNeeded =
        bounds
        |> boundsToTiles Aw3dDefaultTileWidth DemLevel.Level0
        |> Seq.toList

    let aw3dTileResults =
        aw3dTilesNeeded
        |> List.map (fun tileId ->
            tileId,
            ensureAw3dTile
                cacheDir
                fileExists
                downloadFileWithoutRedirects
                readZipFile
                copyStreamToFile
                deleteFile
                openFileToWrite
                tileId)

    let aw3dErrors =
        aw3dTileResults
        |> List.choose (fun (_, result) ->
            match result with
            | Ok _ -> None
            | Error message -> Some message)

    match aw3dErrors with
    | [] -> Result.Ok aw3dTilesNeeded
    | _ -> Result.Error(String.concat "\n" aw3dErrors)



let readAw3dTile cacheDir (tileId: DemTileId) : HeightsArray =
    let fileName = tileId |> aw3dTileCachedTifFileName cacheDir

    use tiff = Tiff.Open(fileName, "r")

    let width = unbox (tiff.GetField(TiffTag.IMAGEWIDTH).[0].Value)

    if width <> Aw3dDefaultTileWidth && width <> Aw3dHighLatitudeTileWidth then
        failwithf
            $"Expected width %d{Aw3dDefaultTileWidth} or %d{Aw3dHighLatitudeTileWidth}, but got %d{width}"

    let height = unbox (tiff.GetField(TiffTag.IMAGELENGTH).[0].Value)

    if height <> Aw3dTileHeight then
        failwithf $"Expected height %d{Aw3dTileHeight}, but got %d{height}"

    let arraySize = width * height
    let heightsArray: DemHeight[] = Array.zeroCreate arraySize

    let samplesPerPixel: int16 =
        unbox (tiff.GetField(TiffTag.SAMPLESPERPIXEL).[0].Value)

    let bitsPerSample: int16 =
        unbox (tiff.GetField(TiffTag.BITSPERSAMPLE).[0].Value)

    let scanlineSize = tiff.ScanlineSize()
    let buffer = Array.zeroCreate<byte> scanlineSize

    let pixelBytesSize = int samplesPerPixel * (int bitsPerSample / 8)

    for row in 0 .. height - 1 do
        // since the coordinate system of the DEM heights array is flipped
        // vertically compared to the bitmap coordinate system, when reading
        // the scanlines, we must put them in the array in reverse order
        let mutable heightsArrayValueIndex = (height - row - 1) * width

        let success = tiff.ReadScanline(buffer, row)

        if not success then
            failwithf $"Failed to read scanline %d{row}"

        // Process the buffer to get the pixel data

        let mutable pixelStart = 0

        for col in 0 .. width - 1 do
            // todo sometime 10: do we already have a function for this?

            // read little-endian int16 value from pixelData
            let height = BitConverter.ToInt16(buffer, pixelStart)

            heightsArray.[heightsArrayValueIndex] <- height
            heightsArrayValueIndex <- heightsArrayValueIndex + 1

            pixelStart <- pixelStart + pixelBytesSize

    // todo 5: how to deal with half-width tiles here?
    let cellMinX, cellMinY =
        tileMinCell
            Aw3dDefaultTileWidth
            { Level = { Value = 0 }
              TileX = tileId.TileX
              TileY = tileId.TileY }

    HeightsArray(
        cellMinX,
        cellMinY,
        width,
        height,
        HeightsArrayDirectImport heightsArray
    )
