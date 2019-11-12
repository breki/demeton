module Tests.Srtm.``Fetching SRTM tiles``.``Checking local cache status``

open Demeton.Srtm.Fetch

open Xunit
open Swensen.Unquote
open Tests.Srtm.SrtmHelper

let localCacheDir = "somecache"

let whenFileExists fileNameThatExists: FileSys.FileExistsChecker 
    = fun fileNameToCheck -> fileNameToCheck = fileNameThatExists

[<Fact>]
let ``If PNG file of level > 0 is not in the cache, it is marked as not cached``() =
    let tile = srtmTileCoords 4 10 20

    test <@ 
            checkLocalCacheTileStatus 
                localCacheDir
                (fun _ -> false)
                tile = LocalCacheTileStatus.NotCached @>

[<Fact>]
let ``If PNG file of level > 0 is in the cache, it is marked as cached``() =
    let tile = srtmTileCoords 4 10 20

    test <@ 
            checkLocalCacheTileStatus 
                localCacheDir
                (whenFileExists 
                    (localCacheDir 
                    |> Pth.combine "4" |> Pth.combine "N20E010.png"))
                tile = LocalCacheTileStatus.Cached @>

[<Fact>]
let ``If PNG file of level 0 is not in the cache, it is marked as not existing``() =
    let tile = srtmTileCoords 0 10 20

    test <@ 
            checkLocalCacheTileStatus 
                localCacheDir
                (whenFileExists 
                    (localCacheDir 
                    |> Pth.combine "0" |> Pth.combine "N20E010.none"))
                tile = LocalCacheTileStatus.NotCached @>

[<Fact>]
let ``If PNG file of level > 0 is not in the cache, but '.none' is, it is marked as not existing``() =
    let tile = srtmTileCoords 4 10 20

    test <@ 
            checkLocalCacheTileStatus 
                localCacheDir
                (whenFileExists 
                    (localCacheDir 
                    |> Pth.combine "4" |> Pth.combine "N20E010.none"))
                tile = LocalCacheTileStatus.HigherLevelDoesNotExist @>

