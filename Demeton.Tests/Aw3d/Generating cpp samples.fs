module Demeton.Tests.Aw3d.Generating_cpp_samples

open System
open System.IO
open Demeton.Dem.Funcs
open Demeton.Aw3d.Funcs
open Text

open FsUnit
open Xunit
open TestHelp
open Swensen.Unquote

[<Literal>]
let CacheDir = "cache"

[<Literal>]
let ScreenWidth = 536

[<Literal>]
let ScreenHeight = 336

[<Fact>]
let ``Generate DEM height data C++ array`` () =
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

    let text = buildString ()
    let minX = 0
    let minY = 0

    let demVarName = sprintf "%s_%04d_%04d" (tileId |> toTileName) minX minY

    text
    |> appendFormat
        @"#include <stdint.h>

int16_t dem{0}[] = {{
"
        [| demVarName |]
    |> ignore

    let mutable comma = ""

    for y in 0 .. ScreenHeight - 1 do
        for x in 0 .. ScreenWidth - 1 do
            let height = heightsArray.heightAtLocal (minX + x, minY + y)

            // write the height in the C-compatible hex format
            text |> appendFormat "{0}0x{1:X4}" [| comma; height |] |> ignore
            comma <- ", "

        text |> appendLine "" |> ignore

    text |> appendLine "};" |> ignore

    // write text to a file
    let fileName = sprintf "%s-%04d-%04d.cpp" (tileId |> toTileName) minX minY

    let path = Path.Combine(CacheDir, fileName)
    File.WriteAllText(path, text.ToString())
