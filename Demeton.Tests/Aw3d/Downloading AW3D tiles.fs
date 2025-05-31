module Tests.Aw3d.Loading_AW3D_tiles

open System.IO

open Demeton.Dem.Types
open Demeton.Geometry.Common
open Demeton.Dem.Funcs
open Demeton.Aw3d.Types
open Demeton.Aw3d.Funcs


open FsUnit
open Xunit
open Swensen.Unquote
open TestHelp

[<Fact>]
let ``Correctly calculates the AW3D tiles needed for a given boundary`` () =
    let bounds =
        { MinLon = 6.9
          MinLat = 46.1
          MaxLon = 8.1
          MaxLat = 47.9 }

    let expectedTiles =
        [ demTileXYId 6 46
          demTileXYId 6 47
          demTileXYId 7 46
          demTileXYId 7 47
          demTileXYId 8 46
          demTileXYId 8 47 ]
        |> Set.ofList

    test
        <@
            (boundsToTiles Aw3dDefaultTileWidth DemLevel.Level0 bounds
             |> Set.ofSeq) = expectedTiles
        @>


[<Theory>]
[<InlineData(6,
             46,
             "https://www.eorc.jaxa.jp/ALOS/aw3d30/data/release_v2404/N045E005/N046E006.zip")>]
[<InlineData(167,
             -48,
             "https://www.eorc.jaxa.jp/ALOS/aw3d30/data/release_v2404/S050E165/S048E167.zip")>]
[<InlineData(-26,
             37,
             "https://www.eorc.jaxa.jp/ALOS/aw3d30/data/release_v2404/N035W030/N037W026.zip")>]
let ``For a given AW3D tile, construct its download URL``
    tileX
    tileY
    expectedUrl
    =
    test <@ aw3dTileDownloadUrl (demTileXYId tileX tileY) = expectedUrl @>


[<Fact>]
let ``Do not download tile if TIFF already in cache`` () =
    let cacheDir = "cache"
    let sampleTileId = demTileXYId 6 -46

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

    let openFileToWrite _ =
        fail "Opening file to write should not have been called"

    let result =
        ensureAw3dTile
            "cache"
            fileExists
            downloadFile
            openZipFileEntry
            copyStreamToFile
            deleteFile
            openFileToWrite
            sampleTileId

    test <@ result |> isOkValue (Some sampleCachedTifFileName) @>

[<Fact>]
let ``Do not download tile if 'none' file already in cache`` () =
    let cacheDir = "cache"
    let sampleTileId = demTileXYId 6 -46

    let sampleCachedTifFileName =
        aw3dTileCachedTifFileName cacheDir sampleTileId

    let sampleCachedNoneFileName =
        sampleCachedTifFileName |> Pth.extension ".none"

    let fileExists =
        function
        | fileName when fileName = sampleCachedNoneFileName -> true
        | fileName when fileName = sampleCachedTifFileName -> false
        | _ -> fail "Unexpected file name"

    let downloadFile _ _ =
        fail "Downloading file should not have been called"

    let openZipFileEntry _ _ =
        fail "Opening ZIP file entries should not have been called"

    let copyStreamToFile _ _ =
        fail "Copying stream to file should not have been called"

    let deleteFile _ =
        fail "Deleting file should not have been called"

    let openFileToWrite _ =
        fail "Opening file to write should not have been called"

    let result =
        ensureAw3dTile
            "cache"
            fileExists
            downloadFile
            openZipFileEntry
            copyStreamToFile
            deleteFile
            openFileToWrite
            sampleTileId

    test <@ result |> isOkValue None @>

[<Fact>]
let ``Download tile ZIP file if TIFF not in cache`` () =
    let cacheDir = "cache"
    let sampleTileId = demTileXYId 6 46

    let expectedCachedZipFileName =
        Path.Combine(cacheDir, Aw3dDirName, $"N046E006.zip")

    let fileExists _ = false

    let mutable zipFileDownloaded = false

    let downloadFile url localFileName =
        if
            url = "https://www.eorc.jaxa.jp/ALOS/aw3d30/data/release_v2404/N045E005/N046E006.zip"
        then
            if localFileName = expectedCachedZipFileName then
                zipFileDownloaded <- true
                Some localFileName
            else
                fail "Unexpected local ZIP file name"
        else
            fail "Unexpected URL"

    let readZipFileEntry _ _ _ = Ok "some-file.tiff"

    let copyStreamToFile fileName _ = Ok fileName

    let deleteFile fileName = Ok fileName

    let openFileToWrite _ =
        fail "Opening file to write should not have been called"

    let result =
        ensureAw3dTile
            "cache"
            fileExists
            downloadFile
            readZipFileEntry
            copyStreamToFile
            deleteFile
            openFileToWrite
            sampleTileId

    test <@ result |> isOk @>
    test <@ zipFileDownloaded @>

[<Fact>]
let ``Extract tile TIFF to the cache`` () =
    let cacheDir = "cache"
    let sampleTileId = demTileXYId 6 46

    let expectedCachedTifFileName =
        Path.Combine("cache", Aw3dDirName, "N046E006.tif")

    let expectedCachedZipFileName =
        Path.Combine(cacheDir, Aw3dDirName, "N046E006.zip")

    let fileExists _ = false

    let downloadFile _ localFileName = Some localFileName

    let readZipFileEntry zipFileName entryName _ =
        if zipFileName = expectedCachedZipFileName then
            if entryName = "N046E006/ALPSMLC30_N046E006_DSM.tif" then
                Ok expectedCachedTifFileName
            else
                fail "Unexpected ZIP file entry name"
        else
            fail "Unexpected ZIP file name"

    let copyStreamToFile fileName _ =
        if fileName = expectedCachedTifFileName then
            Ok fileName
        else
            fail "Unexpected TIFF file name"

    let deleteFile fileName = Ok fileName

    let openFileToWrite _ =
        fail "Opening file to write should not have been called"

    let result =
        ensureAw3dTile
            "cache"
            fileExists
            downloadFile
            readZipFileEntry
            copyStreamToFile
            deleteFile
            openFileToWrite
            sampleTileId

    test <@ result |> isOkValue (Some expectedCachedTifFileName) @>

[<Fact>]
let ``Delete downloaded ZIP file after extraction`` () =
    let cacheDir = "cache"
    let sampleTileId = demTileXYId 6 46

    let expectedCachedTifFileName =
        Path.Combine("cache", Aw3dDirName, "N046E006.tif")

    let expectedCachedZipFileName =
        Path.Combine(cacheDir, Aw3dDirName, "N046E006.zip")

    let fileExists _ = false

    let downloadFile _ localFileName = Some localFileName

    let readZipFileEntry _ _ _ = Ok expectedCachedTifFileName

    let copyStreamToFile fileName _ = Ok fileName

    let zipFileDeleted = ref false

    let deleteFile fileName =
        if fileName = expectedCachedZipFileName then
            zipFileDeleted := true
            Ok fileName
        else
            fail (
                "Unexpected ZIP file name, "
                + $"expected %s{expectedCachedZipFileName}, got %s{fileName}"
            )

    let openFileToWrite _ =
        fail "Opening file to write should not have been called"

    let _ = zipFileDeleted

    let result =
        ensureAw3dTile
            "cache"
            fileExists
            downloadFile
            readZipFileEntry
            copyStreamToFile
            deleteFile
            openFileToWrite
            sampleTileId

    test <@ result |> isOkValue (Some expectedCachedTifFileName) @>

    test
        <@
            ignore "check that the ZIP file was deleted"
            !zipFileDeleted
        @>

/// <summary>
/// If the TIFF tile does not exist at the AW3D server (most likely because
/// it is wholly covered by water), a ".none" file should be created in the
/// cache directory to indicate that.
/// </summary>
[<Fact>]
let ``Create a '.none' file if tile does not exist at server`` () =
    let cacheDir = "cache"
    let sampleTileId = demTileXYId 6 46

    let expectedCachedZipFileName =
        Path.Combine(cacheDir, Aw3dDirName, $"N046E006.zip")

    let fileExists _ = false

    // the download function returns None to indicate that the tile does
    // not exist at the server
    let downloadFile _ _ = None

    let readZipFileEntry _ _ _ =
        fail "Reading ZIP file entry should not have been called"

    let copyStreamToFile _ _ =
        fail "Copying stream to file should not have been called"

    let deleteFile _ =
        fail "Deleting file should not have been called"

    let openFileToWrite fileName =
        if fileName = (expectedCachedZipFileName |> Pth.extension ".none") then
            Ok(new MemoryStream() :> Stream)
        else
            fail "Expected '.none' file name"

    let result =
        ensureAw3dTile
            "cache"
            fileExists
            downloadFile
            readZipFileEntry
            copyStreamToFile
            deleteFile
            openFileToWrite
            sampleTileId

    test <@ result |> isOk @>
