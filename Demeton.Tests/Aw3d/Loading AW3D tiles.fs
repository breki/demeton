﻿module Demeton.Tests.Aw3d.Loading_AW3D_tiles

open Demeton.Dem.Funcs
open Demeton.Aw3d.Types
open Demeton.Aw3d.Funcs
open FsUnit
open Xunit
open TestHelp
open Swensen.Unquote

[<Literal>]
let CacheDir = "cache"

[<Fact>]
let ``Load AW3D into a DemHeight`` () =
    let tileId = demTileXYId 7 46

    let result =
        tileId
        |> ensureAw3dTile
            CacheDir
            FileSys.fileExists
            FileSys.downloadFileWithoutRedirects
            FileSys.readZipFile
            FileSys.copyStreamToFile
            FileSys.deleteFile
            FileSys.openFileToWrite

    test <@ result |> isOk @>

    let heightsArray = readAw3dTile CacheDir tileId

    test <@ heightsArray.Width = Aw3dTileSize @>
    test <@ heightsArray.Height = Aw3dTileSize @>
    test <@ heightsArray.MinX = 7 * 3600 @>
    test <@ heightsArray.MinY = 46 * 3600 @>

[<Fact>]
let ``When trying to load a non-existing AW3D, None is returned`` () =
    let tileId = demTileXYId -32 36

    let result =
        tileId
        |> ensureAw3dTile
            CacheDir
            FileSys.fileExists
            FileSys.downloadFileWithoutRedirects
            FileSys.readZipFile
            FileSys.copyStreamToFile
            FileSys.deleteFile
            FileSys.openFileToWrite

    test <@ result = Ok None @>
