module ``fetchSrtmTile tests``

open Demeton.DemTypes
open Demeton.Srtm

open Xunit
open Swensen.Unquote
open Demeton.SrtmTypes

let srtmDir = "some/srtm/dir"
let localCacheDir = "some/cache"

let mutable zippedTilesInSrtmStorage: (string * HeightsArray) list = []
let mutable pngFilesInLocalCacheDir: (string * HeightsArray) list = []

let init() =
    zippedTilesInSrtmStorage <- []
    pngFilesInLocalCacheDir <- []

let withPngTilePresentInLocalCache tileId =
    let fileContents = HeightsArray(10, 20, 5, 5, (fun _ -> None))
    let fileInCache = (
        (localCacheDir |> Paths.combine tileId |> Paths.extension ".png"),
        fileContents)
    pngFilesInLocalCacheDir <- fileInCache :: pngFilesInLocalCacheDir
    fileContents

let withZippedTilePresentInSrtmStorage tileId =
    let fileContents = HeightsArray(10, 20, 5, 5, (fun _ -> None))
    let fileInCache = (
        (srtmDir |> Paths.combine (sprintf "%s.SRTMGL1.hgt.zip" tileId)),
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
    contents

let pngTileConverter (zippedTileFile: SrtmTileFile) _ =
    let (_, contents) =
        zippedTilesInSrtmStorage
        |> List.find (fun (fileName, _) -> 
            fileName = zippedTileFile.FileName)
    contents

[<Fact>]
let ``If PNG tile is already in local cache, return that one``() =
    init()

    let tileId = "N46E015"
    let tileCoords = parseTileId tileId

    let heightsArray = withPngTilePresentInLocalCache tileId

    let heightsArrayReturned = 
        fetchSrtmTile 
            srtmDir
            localCacheDir
            fileExists
            pngFileReader
            pngTileConverter
            tileCoords

    test <@ heightsArrayReturned = Some heightsArray @>

[<Fact>]
let ``If PNG tile is not in the cache and there is no zipped tile in the SRTM storage, returns None``() =
    init()

    let tileId = "N46E015"
    let tileCoords = parseTileId tileId

    let heightsArrayReturned = 
        fetchSrtmTile 
            srtmDir
            localCacheDir
            fileExists
            pngFileReader
            pngTileConverter
            tileCoords

    test <@ heightsArrayReturned = None @>

[<Fact>]
let ``If PNG tile is not in the cache and there is a zipped tile in the SRTM storage, converts it to PNG and returns it``() =
    init()

    let tileId = "N46E015"
    let tileCoords = parseTileId tileId

    let heightsArray = withZippedTilePresentInSrtmStorage tileId

    let heightsArrayReturned = 
        fetchSrtmTile 
            srtmDir
            localCacheDir
            fileExists
            pngFileReader
            pngTileConverter
            tileCoords

    test <@ heightsArrayReturned = Some heightsArray @>
    