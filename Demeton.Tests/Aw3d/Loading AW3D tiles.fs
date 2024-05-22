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
    let tileId = demTileXYId 46 -7

    let result =
        tileId
        |> ensureAw3dTile
            CacheDir
            FileSys.fileExists
            FileSys.downloadFile
            FileSys.readZipFile
            FileSys.copyStreamToFile
            FileSys.deleteFile

    test <@ result |> isOk @>

    let heightsArray = readAw3dTile CacheDir tileId

    test <@ heightsArray.Width = Aw3dTileSize @>
    test <@ heightsArray.Height = Aw3dTileSize @>
    test <@ heightsArray.MinX = 165600 @>
    test <@ heightsArray.MinY = -28800 @>
