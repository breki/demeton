module Demeton.Tests.Aw3d.Loading_AW3D_tiles

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

    test <@ heightsArray.Width = Aw3dDefaultTileWidth @>
    test <@ heightsArray.Height = Aw3dTileHeight @>
    test <@ heightsArray.MinX = 7 * 3600 @>
    test <@ heightsArray.MinY = 46 * 3600 @>

/// <summary>
/// readAw3dTile supports half-width tiles for high-latitude tiles.
/// </summary>
/// <remarks>
/// In order to avoid having to do a lot of changes in the downstream code,
/// the function now creates a full-width heights array with heights from the
/// original half-width tile filled twice, in pairs.
/// </remarks>
[<Fact>]
// [<Trait("Category", "slow")>]
let ``Load high-latitude AW3D into a DemHeight`` () =
    let tileId = demTileXYId -148 61

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

    test <@ heightsArray.Width = Aw3dDefaultTileWidth @>
    test <@ heightsArray.Height = Aw3dTileHeight @>
    test <@ heightsArray.MinX = -148 * 3600 @>
    test <@ heightsArray.MinY = 61 * 3600 @>

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
