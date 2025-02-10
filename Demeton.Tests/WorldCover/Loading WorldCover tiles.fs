module Tests.WorldCover.Loading_WorldCover_tiles

open Demeton.Geometry.Common
open Demeton.Dem.Funcs
open Demeton.WorldCover.Types
open Demeton.WorldCover.Fetch
open FileSys

open FsUnit
open Xunit
open Swensen.Unquote
open TestHelp

[<Theory>]
[<InlineData(13, 45, 12, 45)>]
[<InlineData(-29, 38, -30, 36)>]
[<InlineData(18, -33, 18, -36)>]
let ``Containing World Cover file ID is correct``
    tileX
    tileY
    expectedContainingTileX
    expectedContainingTileY
    =
    let tileId = demTileXYId tileX tileY

    test
        <@
            containingWorldCoverFileTileId tileId = (demTileXYId
                expectedContainingTileX
                expectedContainingTileY)
        @>


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
let ``Can fetch the list of all available WorldCover files`` () =
    let cacheDir = "cache"

    let geoJsonFile = ensureGeoJsonFile cacheDir fileExists downloadFile

    let allAvailableFiles = listAllAvailableFiles openFileToRead geoJsonFile

    test <@ allAvailableFiles |> Seq.length = 2651 @>


[<Fact>]
let ``Correctly calculates the WorldCover files needed for a given boundary, positive values``
    ()
    =
    let bounds =
        { MinLon = 46.1
          MinLat = 6.9
          MaxLon = 49.9
          MaxLat = 10.1 }

    test
        <@
            boundsToWorldCoverFiles bounds |> Set.ofSeq = set
                [ demTileXYId 45 6
                  demTileXYId 48 6
                  demTileXYId 45 9
                  demTileXYId 48 9 ]
        @>

[<Fact>]
let ``Correctly calculates the WorldCover files needed for a given boundary, negative values``
    ()
    =
    let bounds =
        { MinLon = -5
          MinLat = -5
          MaxLon = -1
          MaxLat = -1 }

    test
        <@
            boundsToWorldCoverFiles bounds |> Set.ofSeq = set
                [ demTileXYId -6 -6
                  demTileXYId -6 -3
                  demTileXYId -3 -6
                  demTileXYId -3 -3 ]
        @>


[<Fact>]
let ``Correctly calculates the WorldCover files needed for a given boundary, around null island``
    ()
    =
    let bounds =
        { MinLon = -2
          MinLat = -2
          MaxLon = 2
          MaxLat = 2 }

    test
        <@
            boundsToWorldCoverFiles bounds |> Set.ofSeq = set
                [ demTileXYId -3 0
                  demTileXYId -3 -3
                  demTileXYId 0 0
                  demTileXYId 0 -3 ]
        @>


[<Fact>]
let ``Do not download tile if TIFF already in cache`` () =
    let cacheDir = "cache"
    let sampleTileId = demTileXYId 46 6

    let sampleCachedTifFileName =
        worldCoverTileCachedTiffFileName cacheDir sampleTileId

    let fileExists =
        function
        | fileName when fileName = sampleCachedTifFileName -> true
        | _ -> fail "Unexpected file name"

    let downloadFile _ _ =
        fail "Downloading file should not have been called"

    let worldCoverPngFileName =
        sampleTileId |> ensureWorldCoverFile "cache" fileExists downloadFile

    test <@ worldCoverPngFileName.Length > 0 @>


[<Fact>]
let ``Download tile file if not in cache`` () =
    let cacheDir = "cache"
    let sampleTileId = demTileXYId 6 -46

    let expectedCachedTiffFileName =
        worldCoverTileCachedTiffFileName cacheDir sampleTileId

    let fileExists _ = false

    let mutable fileDownloaded = false

    let downloadFile url localFileName =
        if
            url = "https://esa-worldcover.s3.eu-central-1.amazonaws.com/"
                  + "v200/2021/map/ESA_WorldCover_10m_2021_v200_S46E006_Map.tif"
        then
            if localFileName = expectedCachedTiffFileName then
                fileDownloaded <- true
                localFileName
            else
                fail "Unexpected local tile file name"
        else
            fail "Unexpected URL"

    let worldCoverPngFileName =
        sampleTileId |> ensureWorldCoverFile "cache" fileExists downloadFile

    test <@ worldCoverPngFileName.Length > 0 @>
    test <@ fileDownloaded @>
