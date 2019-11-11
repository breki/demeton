module Tests.Srtm.``Fetching SRTM tiles``.``Checking local cache status``

open Demeton.Srtm.Types
open Demeton.Srtm.Funcs

open Xunit
open Swensen.Unquote
open Tests.Srtm.SrtmHelper

let localCacheDir = "somecache"

type SrtmDirTileChecker = SrtmLongitude -> SrtmLatitude -> bool
type LocalCacheTileChecker = SrtmTileCoords -> bool

let srtmExistsInSrtmDir: SrtmDirTileChecker = 
    fun lon lat ->
    invalidOp "todo"

type LocalCacheTileStatus =
    | NotCached
    | DoesNotExist
    | Cached

type LocalCacheTileStatusChecker = SrtmTileCoords -> LocalCacheTileStatus

let whenFileExists fileNameThatExists: FileSys.FileExistsChecker 
    = fun fileNameToCheck -> fileNameToCheck = fileNameThatExists

let checkLocalCacheTileStatus
    (localCacheDir: string) (fileExists: FileSys.FileExistsChecker)
    : LocalCacheTileStatusChecker =
    fun tile -> 
        let tileFileName = (toLocalCacheTileFile localCacheDir tile).FileName
        match fileExists tileFileName with
        | true -> Cached
        | false -> 
            let notExistsIndicatorFileName = 
                tileFileName |> Pth.extension ".none"
            match fileExists notExistsIndicatorFileName with
            | true -> DoesNotExist
            | false -> NotCached

//type SrtmTileFetchCase =
//    | Level0NotExists
//    | Level0ExistsNotCached
//    | HigherLevelNotExists
//    | HigherLevelNotCached
//    | Cached

//let srtmTileFetchingCase tileCoords = 
//    Level0NotExists

[<Fact>]
let ``If PNG file is not in the cache, it is marked as not cached``() =
    let tile = srtmTileCoords 4 10 20

    test <@ 
            checkLocalCacheTileStatus 
                localCacheDir
                (fun _ -> false)
                tile = NotCached @>

[<Fact>]
let ``If PNG file is in the cache, it is marked as cached``() =
    let tile = srtmTileCoords 4 10 20

    test <@ 
            checkLocalCacheTileStatus 
                localCacheDir
                (whenFileExists 
                    (localCacheDir 
                    |> Pth.combine "4" |> Pth.combine "N20E010.png"))
                tile = Cached @>

[<Fact>]
let ``If PNG file is not in the cache, but '.none' is, it is marked as not existing``() =
    let tile = srtmTileCoords 4 10 20

    test <@ 
            checkLocalCacheTileStatus 
                localCacheDir
                (whenFileExists 
                    (localCacheDir 
                    |> Pth.combine "4" |> Pth.combine "N20E010.none"))
                tile = DoesNotExist @>

//[<Fact (Skip="todo")>]
//let ``Handles the case when level 0 SRTM tile neither exists in the cache nor in the store``() =
//    let tile = srtmTileCoords 0 10 20
//    test <@ srtmTileFetchingCase tile = Level0NotExists @>

//[<Fact (Skip="todo")>]
//let ``Handles the case when level 0 SRTM tile is not cached``() = 
//    invalidOp "todo"    
