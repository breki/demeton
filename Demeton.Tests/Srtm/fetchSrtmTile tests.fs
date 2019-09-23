module ``fetchSrtmTile tests``

open Demeton.DemTypes
open Demeton.Srtm

open Xunit
open Swensen.Unquote

let localCacheDir = "some/cache"

let mutable pngFilesInLocalCacheDir: (string * HeightsArray) list = []

let withPngTilePresentInLocalCache tileId =
    let fileContents = HeightsArray(10, 20, 5, 5, (fun _ -> None))
    let fileInCache = (
        (localCacheDir |> Paths.combine tileId |> Paths.extension ".png"),
        fileContents)
    pngFilesInLocalCacheDir <- fileInCache :: pngFilesInLocalCacheDir
    fileContents

let fileExists fileNameToFind =
    pngFilesInLocalCacheDir 
    |> List.exists (fun (fileName, _) -> fileName = fileNameToFind)

let pngFileReader fileNameToFind =
    let (_, contents) =
        pngFilesInLocalCacheDir
        |> List.find (fun (fileName, _) -> fileName = fileNameToFind)
    contents

[<Fact>]
let ``If PNG tile is already in local cache, return that one``() =
    let tileId = "N46E015"
    let tileCoords = parseTileId tileId

    let heightsArray = withPngTilePresentInLocalCache tileId

    let heightsArrayReturned = 
        fetchSrtmTile 
            localCacheDir
            fileExists
            pngFileReader
            tileCoords

    test <@ heightsArrayReturned = Some heightsArray @>
