﻿/// <summary>
/// Tests the behavior of code for saving SRTM tiles into the local cache,
/// whether it saves it as a PNG file or a '.none' file.
/// </summary>
module Tests.Srtm.``Saving SRTM tiles to cache``

open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Demeton.Srtm.Funcs
open Demeton.Srtm.Png

open Xunit
open Swensen.Unquote
open TestHelp
open System.IO

let cacheDir = "somecache"

let heights =
    HeightsArray(1, 2, 3, 4, HeightsArrayInitializer1D(fun _ -> DemHeightNone))

let mutable pngFileName = None
let mutable noneFileName = None
let mutable createdCacheDirectory = None

let ensureCacheDirectoryExists: FileSys.DirectoryExistsEnsurer =
    fun dir ->
        createdCacheDirectory <- Some dir
        Ok dir

let writeAsPngFile: HeightsArrayPngWriter =
    fun fileName heightsArray ->
        pngFileName <- Some fileName
        heightsArray |> Ok

let writeAsNoneFile: FileSys.FileWriter =
    fun fileName ->
        noneFileName <- Some fileName
        new MemoryStream() :> Stream |> Ok

// When saving a tile to the cache, it writes it into an appropriate location
// in the cache, as a PNG file.
[<Fact>]
let ``Saves the tile into the cache directory`` () =
    let tile = demTileId 0 10 -20

    let expectedFileName = tile |> toLocalCacheTileFileName cacheDir

    let writtenTileIdMaybe =
        writeSrtmTileToLocalCache
            cacheDir
            ensureCacheDirectoryExists
            writeAsPngFile
            Should.notBeCalled
            tile
            (Some heights)

    test <@ writtenTileIdMaybe = Ok(Some(tile, heights)) @>
    test <@ pngFileName = Some expectedFileName @>

[<Fact>]
let ``Ensures the cache directory exists before writing the file`` () =
    let tile = demTileId 2 10 -20

    let expectedCreatedCacheDirectory =
        tile
        |> toLocalCacheTileFileName cacheDir
        |> Pth.extension ".none"
        |> Pth.directory

    writeSrtmTileToLocalCache
        cacheDir
        ensureCacheDirectoryExists
        writeAsPngFile
        writeAsNoneFile
        tile
        None
    |> ignore

    test <@ createdCacheDirectory = Some expectedCreatedCacheDirectory @>


// When trying to write a tile of level 0 that has no heights array, simply
// ignore it and do nothing.
[<Fact>]
let ``Ignores the non-existing tile level 0`` () =
    let tile = demTileId 0 10 -20

    let writtenTileIdMaybe =
        writeSrtmTileToLocalCache
            cacheDir
            ensureCacheDirectoryExists
            writeAsPngFile
            writeAsNoneFile
            tile
            None

    test <@ writtenTileIdMaybe = Ok None @>
    test <@ pngFileName = None @>
    test <@ noneFileName = None @>

// Creates an empty ".none" file for level > 0 tile that does not have any
// heights, which is used as an indicator for non-existing tiles by the fetch
// tiles mechanism.
[<Fact>]
let ``Saves the non-existing tile info into the cache directory`` () =
    let tile = demTileId 2 10 -20

    let expectedFileName =
        tile |> toLocalCacheTileFileName cacheDir |> Pth.extension ".none"

    let writtenTileIdMaybe =
        writeSrtmTileToLocalCache
            cacheDir
            ensureCacheDirectoryExists
            writeAsPngFile
            writeAsNoneFile
            tile
            None

    test <@ writtenTileIdMaybe = Ok None @>
    test <@ noneFileName = Some expectedFileName @>
    test <@ pngFileName = None @>
