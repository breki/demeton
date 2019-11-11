module Tests.Srtm.``Fetching SRTM tiles old``

open Demeton.DemTypes
open Demeton.Srtm
open Demeton.Srtm.Types
open Demeton.Srtm.Funcs

open FsUnit
open Xunit
open Swensen.Unquote
open TestHelp

let srtmDir = "some/srtm/dir"
let localCacheDir = "some/cache"

let mutable zippedTilesInSrtmStorage: (string * HeightsArray) list = []
let mutable pngFilesInLocalCacheDir: (string * HeightsArray) list = []

let someHeightsArray minX minY width height =
    HeightsArray(
        minX, minY, width, height, 
        HeightsArrayInitializer1D (fun _ -> DemHeightNone))

let init() =
    zippedTilesInSrtmStorage <- []
    pngFilesInLocalCacheDir <- []

let withPngTilePresentInLocalCache (tileCoords: SrtmTileCoords) =
    let fileContents = someHeightsArray 10 20 5 5

    let tileId = Tile.tileId tileCoords
    let fileInCache = (
        (localCacheDir |> Pth.combine (tileCoords.Level.ToString()) 
        |> Pth.combine tileId |> Pth.extension ".png"),
        fileContents)
    pngFilesInLocalCacheDir <- fileInCache :: pngFilesInLocalCacheDir
    fileContents

let withZippedTilePresentInSrtmStorage tileId =
    let fileContents = someHeightsArray 10 20 5 5
    let fileInCache = (
        (srtmDir |> Pth.combine (sprintf "%s.SRTMGL1.hgt.zip" tileId)),
        fileContents)
    zippedTilesInSrtmStorage <- fileInCache :: zippedTilesInSrtmStorage
    fileContents

let fileExistsInSrtmDir fileNameToFind =
    zippedTilesInSrtmStorage
    |> List.exists (fun (fileName, _) -> fileName = fileNameToFind)

let fileExistsInLocalDir fileNameToFind =
    pngFilesInLocalCacheDir 
    |> List.exists (fun (fileName, _) -> fileName = fileNameToFind)

let fileExists fileNameToFind = 
    fileExistsInLocalDir fileNameToFind || fileExistsInSrtmDir fileNameToFind

let pngFileReader fileNameToFind =
    let (_, contents) =
        pngFilesInLocalCacheDir
        |> List.find (fun (fileName, _) -> fileName = fileNameToFind)
    Ok contents

let pngTileConverter _ zippedTileFileName _ =
    let (_, contents) =
        zippedTilesInSrtmStorage
        |> List.find (fun (fileName, _) -> fileName = zippedTileFileName)
    contents

[<Fact>]
let ``If PNG tile is already in local cache, return that one``() =
    init()

    let tileId = "N46E015"
    let tileCoords = Tile.parseTileId 0 tileId

    let heightsArray = withPngTilePresentInLocalCache tileCoords

    let heightsArrayReturned = 
        fetchSrtmTile 
            srtmDir
            localCacheDir
            fileExists
            pngFileReader
            (fun _ _ -> invalidOp "bug")
            pngTileConverter
            (fun _ -> invalidOp "bug")
            tileCoords

    test <@ heightsArrayReturned = Ok (Some heightsArray) @>

[<Fact>]
let ``If PNG tile is not in the cache and there is no zipped tile in the SRTM storage, returns None``() =
    init()

    let tileId = "N46E015"
    let tileCoords = Tile.parseTileId 0 tileId

    let heightsArrayReturned = 
        fetchSrtmTile 
            srtmDir
            localCacheDir
            fileExists
            pngFileReader
            (fun _ _ -> invalidOp "bug")
            pngTileConverter
            (fun _ -> invalidOp "bug")
            tileCoords

    test <@ heightsArrayReturned = Ok None @>

[<Fact>]
let ``If PNG tile is not in the cache and there is a zipped tile in the SRTM storage, converts it to PNG and returns it``() =
    init()

    let tileId = "N46E015"
    let tileCoords = Tile.parseTileId 0 tileId

    let heightsArray = withZippedTilePresentInSrtmStorage tileId

    let heightsArrayReturned = 
        fetchSrtmTile 
            srtmDir
            localCacheDir
            fileExists
            pngFileReader
            (fun _ _ -> invalidOp "bug")
            pngTileConverter
            (fun _ -> invalidOp "bug")
            tileCoords

    test <@ heightsArrayReturned = Ok (Some heightsArray) @>    

[<Fact>]
let ``Creates higher-level PNG tiles from lower level ones by resampling them``() =
    let srtmLevel = 1
    let tileId = "N02E008"

    let mutable resampledHeightsArrayProduced = None

    let resamplerWasCalled: HeightsArrayResampler = fun heightsArray ->
        let resampledHeightsArray = someHeightsArray 30 20 5 5
        resampledHeightsArrayProduced <- Some resampledHeightsArray
        resampledHeightsArray

    let pngWriterWasCalled: SrtmPngTileWriter = 
        fun fileName heightsArray ->
        test <@ fileName = 
                    (localCacheDir 
                    |> Pth.combine "1" |> Pth.combine "N02E008.png") @>

        Some heightsArray |> should equal resampledHeightsArrayProduced
        heightsArray

    init()

    let tileCoords = Tile.parseTileId srtmLevel tileId

    let lowerTilesIds = [| "N02E008"; "N03E008"; "N02E009"; "N03E009" |]
    lowerTilesIds
    |> Array.map (fun x -> Tile.parseTileId 0 x)
    |> Array.map withPngTilePresentInLocalCache
    |> ignore

    let heightsArrayReturned = 
        fetchSrtmTile 
            srtmDir
            localCacheDir
            fileExists
            pngFileReader
            pngWriterWasCalled
            pngTileConverter
            resamplerWasCalled
            tileCoords

    test <@ heightsArrayReturned |> isOkValue resampledHeightsArrayProduced @>   
