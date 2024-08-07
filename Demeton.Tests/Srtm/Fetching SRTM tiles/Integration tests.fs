﻿module Tests.Srtm.``Fetching SRTM tiles``.``Integration tests``

open Demeton.Dem.Funcs
open Demeton.Srtm.Fetch
open Demeton.Srtm.Png
open Demeton.Srtm.Downsampling

open System

open Xunit
open Swensen.Unquote
open TestHelp

let private srtmDir = "srtm"
let private cacheDir = Environment.GetEnvironmentVariable("SRTM_CACHE")

let private readPngTile: DemPngTileReader =
    fun fileName -> decodeSrtmTileFromPngFile FileSys.openFileToRead fileName

let private determineTileStatus =
    determineTileStatus srtmDir cacheDir FileSys.fileExists

let private writeHeightsArrayToPng =
    writeHeightsArrayIntoPngFile
        FileSys.ensureDirectoryExists
        FileSys.openFileToWrite

let private readHgtStream = readZippedHgtFile FileSys.readZipFile

let private convertToPng =
    convertZippedHgtTileToPng readHgtStream writeHeightsArrayToPng

let private constructHigherLevelTile = constructHigherLevelTileHeightsArray 3600

let private heightsArrayToPng =
    writeHeightsArrayIntoPngFile
        FileSys.ensureDirectoryExists
        FileSys.openFileToWrite

let private writeTileToCache =
    writeSrtmTileToLocalCache
        cacheDir
        FileSys.ensureDirectoryExists
        heightsArrayToPng
        FileSys.openFileToWrite

[<Fact>]
[<Trait("Category", "slow")>]
let ``Supports fetching already cached tile`` () =
    // Skip this test if the SRTM_DIR environment variable is not set.
    // This is a current workaround for GitHub action not getting the
    // variable set.
    match cacheDir with
    | null -> ()
    | _ ->
        let finalState =
            initializeProcessingState (demTileId 0 15 -46)
            |> processCommandStack
                cacheDir
                srtmDir
                determineTileStatus
                readPngTile
                convertToPng
                constructHigherLevelTile
                writeTileToCache

        let result = finalState |> finalizeFetchSrtmTileProcessing
        test <@ result |> isOk @>
