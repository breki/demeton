module Tests.WorldCover.Loading_WorldCover_tiles

open System.IO
open Demeton.Geometry.Common
open Demeton.WorldCover.Types
open Demeton.WorldCover.Funcs
open FileSys

open FsUnit
open Xunit
open Swensen.Unquote
open TestHelp


[<Fact>]
let ``Download WorldCover geoJSON file if it is not already cached`` () =
    let cacheDir = "cache"

    let geoJsonCachedFileName = worldCoverGeoJsonCachedFileName cacheDir

    let fileExists =
        function
        | fileName when fileName = geoJsonCachedFileName -> false
        | _ -> fail "Unexpected file name"

    let mutable downloadFileCalls = 0

    let downloadFile (url: string) (fileName: string) =
        match url, fileName with
        | url, fileName when
            url = geoJsonUrl && fileName = geoJsonCachedFileName
            ->
            downloadFileCalls <- downloadFileCalls + 1
            geoJsonCachedFileName
        | _ -> fail "Unexpected URL or file name"


    let resultingFileName = ensureGeoJsonFile "cache" fileExists downloadFile

    test <@ resultingFileName = geoJsonCachedFileName @>
    test <@ downloadFileCalls = 1 @>


[<Fact>]
let ``Skip downloading WorldCover geoJSON file if it is already cached`` () =
    let cacheDir = "cache"

    let geoJsonCachedFileName = worldCoverGeoJsonCachedFileName cacheDir

    let fileExists =
        function
        | fileName when fileName = geoJsonCachedFileName -> true
        | _ -> fail "Unexpected file name"

    let downloadFile (url: string) (fileName: string) =
        fail "Should not be called"


    let resultingFileName = ensureGeoJsonFile "cache" fileExists downloadFile

    test <@ resultingFileName = geoJsonCachedFileName @>


[<Fact>]
let ``Can fetch the list of all available WorldCover tiles`` () =
    let cacheDir = "cache"

    let geoJsonFile = ensureGeoJsonFile cacheDir fileExists downloadFile

    let allAvailableTiles = listAllAvailableTiles openFileToRead geoJsonFile

    test <@ allAvailableTiles |> Seq.length = 2631 @>


[<Fact>]
let ``Correctly calculates the WorldCover tiles needed for a given boundary, positive values``
    ()
    =
    let bounds =
        { MinLon = 46.1
          MinLat = 6.9
          MaxLon = 49.9
          MaxLat = 10.1 }

    test
        <@
            boundsToWorldCoverTiles bounds |> Set.ofSeq = set
                [ { TileX = 45; TileY = -6 }
                  { TileX = 48; TileY = -6 }
                  { TileX = 45; TileY = -9 }
                  { TileX = 48; TileY = -9 } ]
        @>

[<Fact>]
let ``Correctly calculates the WorldCover tiles needed for a given boundary, negative values``
    ()
    =
    let bounds =
        { MinLon = -5
          MinLat = -5
          MaxLon = -1
          MaxLat = -1 }

    test
        <@
            boundsToWorldCoverTiles bounds |> Set.ofSeq = set
                [ { TileX = -6; TileY = 6 }
                  { TileX = -6; TileY = 3 }
                  { TileX = -3; TileY = 6 }
                  { TileX = -3; TileY = 3 } ]
        @>

[<Fact>]
let ``Correctly calculates the WorldCover tiles needed for a given boundary, around null island``
    ()
    =
    let bounds =
        { MinLon = -2
          MinLat = -2
          MaxLon = 2
          MaxLat = 2 }

    test
        <@
            boundsToWorldCoverTiles bounds |> Set.ofSeq = set
                [ { TileX = -3; TileY = 0 }
                  { TileX = -3; TileY = 3 }
                  { TileX = 0; TileY = 0 }
                  { TileX = 0; TileY = 3 } ]
        @>


// todo 10: implement downloading of WorldCover data

/// <summary>
/// Returns the download URL for the given WorldCover tile.
/// </summary>
let worldCoverTileDownloadUrl (tileId: WorldCoverTileId) : string =
    sprintf
        "%s/%s/%i/map/ESA_WorldCover_10m_%i_%s_%s_Map.tif"
        WorldCoverS3Domain
        WorldCoverVersion
        WorldCoverYear
        WorldCoverYear
        WorldCoverVersion
        tileId.TileName

/// <summary>
/// Returns the path to the cached TIFF file for the given AW3D tile.
/// </summary>
let worldCoverTileCachedTifFileName cacheDir (tileId: WorldCoverTileId) =
    Path.Combine(cacheDir, WorldCoverDirName, $"{tileId.TileName}.tif")



/// <summary>
/// Ensures the specified AW3D tile TIFF file is available in the cache
/// directory, downloading it if necessary.
/// </summary>
let ensureWorldCoverTile cacheDir fileExists downloadFile tileId =
    let cachedTifFileName = worldCoverTileCachedTifFileName cacheDir tileId

    if fileExists cachedTifFileName then
        Ok cachedTifFileName
    else
        // download the tile
        let tileUrl = tileId |> worldCoverTileDownloadUrl
        downloadFile tileUrl cachedTifFileName |> Ok



[<Fact>]
let ``Do not download tile if TIFF already in cache`` () =
    let cacheDir = "cache"
    let sampleTileId = { TileX = 46; TileY = 6 }

    let sampleCachedTifFileName =
        worldCoverTileCachedTifFileName cacheDir sampleTileId

    let fileExists =
        function
        | fileName when fileName = sampleCachedTifFileName -> true
        | _ -> fail "Unexpected file name"

    let downloadFile _ _ =
        fail "Downloading file should not have been called"

    let result =
        sampleTileId |> ensureWorldCoverTile "cache" fileExists downloadFile

    test <@ result |> isOkValue sampleCachedTifFileName @>


[<Fact>]
let ``Download tile file if not in cache`` () =
    let cacheDir = "cache"
    let sampleTileId = { TileX = 46; TileY = 6 }

    let expectedCachedTiffFileName =
        worldCoverTileCachedTifFileName cacheDir sampleTileId

    let fileExists _ = false

    let mutable fileDownloaded = false

    let downloadFile url localFileName =
        if
            url = "https://esa-worldcover.s3.eu-central-1.amazonaws.com/"
                  + "v200/2021/map/ESA_WorldCover_10m_2021_v200_N046E006_Map.tif"
        then
            if localFileName = expectedCachedTiffFileName then
                fileDownloaded <- true
                localFileName
            else
                fail "Unexpected local tile file name"
        else
            fail "Unexpected URL"

    let result =
        sampleTileId |> ensureWorldCoverTile "cache" fileExists downloadFile

    test <@ result |> isOk @>
    test <@ fileDownloaded @>


// todo 10: remove this once we have the general download function implemented
let ``Download a sample WorldCover tile`` () =
    let tile = "S48E168"

    let url =
        $"{WorldCoverS3Domain}/{WorldCoverVersion}/{WorldCoverYear}/map/ESA_WorldCover_10m_{WorldCoverYear}_{WorldCoverVersion}_{tile}_Map.tif"

    downloadFile url $"c:/temp/{WorldCoverDirName}/{tile}.tif" |> ignore
