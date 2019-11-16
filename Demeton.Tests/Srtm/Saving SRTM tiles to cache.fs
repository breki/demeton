/// <summary>
/// Tests the behavior of code for saving SRTM tiles into the local cache,
/// whether it saves it as a PNG file or a '.none' file.
/// </summary>
module Tests.Srtm.``Saving SRTM tiles to cache``

open Demeton.DemTypes
open Demeton.Srtm.Funcs
open Demeton.Srtm.Png

open Xunit
open Swensen.Unquote
open Tests.Srtm.SrtmHelper
open TestHelp
open System.IO

let cacheDir = "somecache"
let heights = 
    HeightsArray(1, 2, 3, 4, HeightsArrayInitializer1D (fun _ -> DemHeightNone))

let mutable pngFileName = None
let mutable noneFileName = None

let writeAsPngFile: HeightsArrayPngWriter = 
    fun fileName heightsArray ->
    pngFileName <- Some fileName
    heightsArray

let writeAsNoneFile: FileSys.FileOpener = 
    fun fileName ->
    noneFileName <- Some fileName
    new MemoryStream() :> Stream

// When saving a tile to the cache, it writes it into an appropriate location
// in the cache, as a PNG file.
[<Fact>]
let ``Saves the tile into the cache directory``() =
    let tile = srtmTileId 0 10 -20

    let expectedFileName = tile |> toLocalCacheTileFileName cacheDir

    writeSrtmTileToLocalCache 
        cacheDir
        writeAsPngFile
        _noCall
        tile (Some heights)

    test <@ pngFileName = Some expectedFileName @>

// When trying to write a tile of level 0 that has no heights array, simply 
// ignore it and do nothing.
[<Fact>]
let ``Ignores the non-existing tile leve 0``() =
    let tile = srtmTileId 0 10 -20

    writeSrtmTileToLocalCache 
        cacheDir
        writeAsPngFile
        writeAsNoneFile
        tile None

    test <@ pngFileName = None @>
    test <@ noneFileName = None @>

// Creates an empty ".none" file for level > 0 tile that does not have any 
// heights, which is used as an indicator for non-existing tiles by the fetch
// tiles mechanism.
[<Fact>]
let ``Saves the non-existing tile info into the cache directory``() =
    let tile = srtmTileId 2 10 -20

    let expectedFileName = 
        tile |> toLocalCacheTileFileName cacheDir
        |> Pth.extension ".none"

    writeSrtmTileToLocalCache 
        cacheDir
        writeAsPngFile
        writeAsNoneFile
        tile None

    test <@ noneFileName = Some expectedFileName @>
    test <@ pngFileName = None @>
