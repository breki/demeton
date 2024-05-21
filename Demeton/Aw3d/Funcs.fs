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
/// Given a bounding box, returns a sequence of AW3D tiles that cover it.
/// </summary>
let boundsToAw3dTiles (bounds: LonLatBounds) : DemTileId seq =
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
                yield demTileXYId tileX tileY
    }

/// <summary>
/// Returns the download URL for the given AW3D tile.
/// </summary>
let aw3dTileDownloadUrl (tileId: DemTileId) : string =
    let groupTileX = tileId.TileX / 5 * 5
    let groupTileY = tileId.TileY / 5 * 5

    let groupTileId = demTileXYId groupTileX groupTileY

    sprintf
        "https://www.eorc.jaxa.jp/ALOS/aw3d30/data/release_v2303/%s/%s.zip"
        groupTileId.FormatLat3Lon3
        tileId.FormatLat3Lon3


/// <summary>
/// Returns the path to the cached ZIP file for the given AW3D tile.
/// </summary>
let aw3dTileCachedZipFileName cacheDir (tileId: DemTileId) =
    Path.Combine(cacheDir, Aw3dDirName, $"{tileId.FormatLat3Lon3}.zip")

/// <summary>
/// Returns the path to the cached TIFF file for the given AW3D tile.
/// </summary>
let aw3dTileCachedTifFileName cacheDir (tileId: DemTileId) =
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
            $"Downloading AW3D tile %s{tileId.FormatLat3Lon3} from %s{url}..."

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

    let aw3dTilesNeeded = bounds |> boundsToAw3dTiles |> Seq.toList

    let aw3dTileResults =
        aw3dTilesNeeded
        |> List.map (fun tileId ->
            tileId,
            ensureAw3dTile
                cacheDir
                fileExists
                downloadFile
                readZipFile
                copyStreamToFile
                deleteFile
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

    if width <> Aw3dTileSize then
        failwithf $"Expected width %d{Aw3dTileSize}, but got %d{width}"

    let height = unbox (tiff.GetField(TiffTag.IMAGELENGTH).[0].Value)

    if height <> Aw3dTileSize then
        failwithf $"Expected height %d{Aw3dTileSize}, but got %d{height}"

    let arraySize = width * height
    let heightsArray: DemHeight[] = Array.zeroCreate arraySize

    let samplesPerPixel: int16 =
        unbox (tiff.GetField(TiffTag.SAMPLESPERPIXEL).[0].Value)

    let bitsPerSample: int16 =
        unbox (tiff.GetField(TiffTag.BITSPERSAMPLE).[0].Value)

    let scanlineSize = tiff.ScanlineSize()
    let buffer = Array.zeroCreate<byte> scanlineSize

    let pixelBytesSize = int samplesPerPixel * (int bitsPerSample / 8)

    let mutable heightsWrittenCount = 0

    for row in 0 .. height - 1 do
        let success = tiff.ReadScanline(buffer, row)

        if not success then
            failwithf $"Failed to read scanline %d{row}"

        // Process the buffer to get the pixel data

        let mutable pixelStart = 0

        for col in 0 .. width - 1 do
            // todo 5: do we already have a function for this?

            // read little-endian int16 value from pixelData
            let height = BitConverter.ToInt16(buffer, pixelStart)

            heightsArray.[heightsWrittenCount] <- height
            heightsWrittenCount <- heightsWrittenCount + 1

            pixelStart <- pixelStart + pixelBytesSize

    let cellMinX, cellMinY =
        tileMinCell
            Aw3dTileSize
            { Level = { Value = 0 }
              TileX = tileId.TileX
              TileY = tileId.TileY }

    HeightsArray(
        cellMinX,
        cellMinY,
        Aw3dTileSize,
        Aw3dTileSize,
        HeightsArrayDirectImport heightsArray
    )
