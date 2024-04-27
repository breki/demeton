module Tests.Aw3d.Loading_AW3D_tiles

open System.IO

open Demeton.Geometry.Common
open Demeton.Aw3d.Types
open Demeton.Aw3d.Funcs


open FsUnit
open Xunit
open Swensen.Unquote
open TestHelp


[<Fact>]
let ``Correctly calculates the AW3D tiles needed for a given boundary`` () =
    let bounds =
        { MinLon = 46.1
          MinLat = -8.1
          MaxLon = 47.9
          MaxLat = -6.9 }

    test
        <@
            boundsToAw3dTiles bounds |> Seq.toList = [ { TileX = 46; TileY = 6 }
                                                       { TileX = 47; TileY = 6 }
                                                       { TileX = 46; TileY = 7 }
                                                       { TileX = 47; TileY = 7 }
                                                       { TileX = 46; TileY = 8 }
                                                       { TileX = 47; TileY = 8 } ]
        @>


[<Theory>]
[<InlineData(46,
             6,
             "https://www.eorc.jaxa.jp/ALOS/aw3d30/data/release_v2303/N045E005/N046E006.zip")>]
[<InlineData(-36,
             -120,
             "https://www.eorc.jaxa.jp/ALOS/aw3d30/data/release_v2303/S035W120/S036W120.zip")>]
let ``For a given AW3D tile, construct its download URL``
    tileX
    tileY
    expectedUrl
    =
    test
        <@ aw3dTileDownloadUrl { TileX = tileX; TileY = tileY } = expectedUrl @>


// todo 0: move the AW3D code to the main project

// todo 6: for a given list of AW3D tiles and the cache dir, download all
//   missing ones

[<Literal>]
let Aw3dDirName = "AW3D"


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
    openZipFileEntry
    copyStreamToFile
    deleteFile
    tileId
    =
    let cachedTifFileName = aw3dTileCachedTifFileName cacheDir tileId

    if fileExists cachedTifFileName then
        Ok cachedTifFileName
    else
        // download the tile
        let url = aw3dTileDownloadUrl tileId

        let tileCachedZipFileName = tileId |> aw3dTileCachedZipFileName cacheDir

        let tiffFileNameInZip = aw3dTileZipFileEntryName tileId

        openZipFileEntry tileCachedZipFileName tiffFileNameInZip
        |> Result.bind (fun zippedStream ->
            let tiffFilePath = tileId |> aw3dTileCachedTifFileName cacheDir

            let tiffFilePath = zippedStream |> copyStreamToFile tiffFilePath

            deleteFile tileCachedZipFileName

            Ok tiffFilePath)

[<Fact>]
let ``Do not download tile if TIFF already in cache`` () =
    let cacheDir = "cache"
    let sampleTileId = { TileX = 46; TileY = 6 }

    let sampleCachedTifFileName =
        aw3dTileCachedTifFileName cacheDir sampleTileId

    let fileExists =
        function
        | fileName when fileName = sampleCachedTifFileName -> true
        | _ -> fail "Unexpected file name"

    let downloadFile _ _ =
        fail "Downloading file should not have been called"

    let openZipFileEntry _ _ =
        fail "Opening ZIP file entries should not have been called"

    let copyStreamToFile _ _ =
        fail "Copying stream to file should not have been called"

    let deleteFile _ =
        fail "Deleting file should not have been called"

    let result =
        ensureAw3dTile
            "cache"
            fileExists
            downloadFile
            openZipFileEntry
            copyStreamToFile
            deleteFile
            sampleTileId

    test <@ result |> isOkValue sampleCachedTifFileName @>

[<Fact>]
let ``Download tile ZIP file if TIFF not in cache`` () =
    let cacheDir = "cache"
    let sampleTileId = { TileX = 46; TileY = 6 }

    let expectedCachedZipFileName =
        Path.Combine(cacheDir, Aw3dDirName, $"N046E006.zip")

    let fileExists _ = false

    let downloadFile url localFileName =
        if
            url = "https://www.eorc.jaxa.jp/ALOS/aw3d30/data/release_v2303/N046E006/N046E006.zip"
        then
            if localFileName = expectedCachedZipFileName then
                localFileName
            else
                fail "Unexpected local ZIP file name"
        else
            fail "Unexpected URL"

    let openZipFileEntry _ _ = Ok(new MemoryStream())

    let copyStreamToFile fileName _ = fileName

    let deleteFile _ = ()

    let result =
        ensureAw3dTile
            "cache"
            fileExists
            downloadFile
            openZipFileEntry
            copyStreamToFile
            deleteFile
            sampleTileId

    test <@ result |> isOk @>

[<Fact>]
let ``Extract tile TIFF to the cache`` () =
    let cacheDir = "cache"
    let sampleTileId = { TileX = 46; TileY = 6 }

    let expectedCachedTifFileName =
        Path.Combine("cache", Aw3dDirName, "N046E006.tif")

    let expectedCachedZipFileName =
        Path.Combine(cacheDir, Aw3dDirName, $"N046E006.zip")

    let fileExists _ = false

    let downloadFile url localFileName = localFileName

    let openZipFileEntry zipFileName entryName =
        if zipFileName = expectedCachedZipFileName then
            if entryName = "N046E006/ALPSMLC30_N046E006_DSM.tif" then
                Ok(new MemoryStream())
            else
                fail "Unexpected ZIP file entry name"
        else
            fail "Unexpected ZIP file name"

    let copyStreamToFile fileName _ =
        if fileName = expectedCachedTifFileName then
            fileName
        else
            fail "Unexpected TIFF file name"

    let deleteFile _ = ()

    let result =
        ensureAw3dTile
            "cache"
            fileExists
            downloadFile
            openZipFileEntry
            copyStreamToFile
            deleteFile
            sampleTileId

    test <@ result |> isOkValue expectedCachedTifFileName @>

[<Fact>]
let ``Delete downloaded ZIP file after extraction`` () =
    let cacheDir = "cache"
    let sampleTileId = { TileX = 46; TileY = 6 }

    let expectedCachedTifFileName =
        Path.Combine("cache", Aw3dDirName, "N046E006.tif")

    let expectedCachedZipFileName =
        Path.Combine(cacheDir, Aw3dDirName, "N046E006.zip")

    let fileExists _ = false

    let downloadFile _ localFileName = localFileName

    let openZipFileEntry _ _ = Ok(new MemoryStream())

    let copyStreamToFile fileName _ = fileName

    let zipFileDeleted = ref false

    let deleteFile fileName =
        if fileName = expectedCachedZipFileName then
            zipFileDeleted := true
            ()
        else
            fail "Unexpected ZIP file name"

    let result =
        ensureAw3dTile
            "cache"
            fileExists
            downloadFile
            openZipFileEntry
            copyStreamToFile
            deleteFile
            sampleTileId

    test <@ result |> isOkValue expectedCachedTifFileName @>

    test
        <@
            ignore "check that the ZIP file was deleted"
            !zipFileDeleted
        @>
