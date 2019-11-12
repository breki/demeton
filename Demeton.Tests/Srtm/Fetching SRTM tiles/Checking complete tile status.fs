module Tests.Srtm.``Fetching SRTM tiles``.``Checking complete tile status``

open Demeton.Srtm.Types
open Demeton.Srtm.Fetch

open Xunit
open Swensen.Unquote

let tileIsNotInSrtmDir = 
    Lazy<SrtmDirTileStatus>(fun () -> SrtmDirTileStatus.DoesNotExist)

let tileIsInSrtmDir = 
    Lazy<SrtmDirTileStatus>(fun () -> SrtmDirTileStatus.Exists)

let doNotCallMe = 
    Lazy<SrtmDirTileStatus>(fun () -> invalidOp "should not be called")

[<Fact>]
let ``When level 0 tile neither exists in the cache nor in the store``() =
    test <@ decideSrtmTileStatus 
                (SrtmLevel.fromInt 0) 
                LocalCacheTileStatus.NotCached tileIsNotInSrtmDir 
                = NotExists @>

[<Fact>]
let ``When level 0 tile is not cached but it exists in SRTM dir``() = 
    test <@ decideSrtmTileStatus 
                (SrtmLevel.fromInt 0) 
                LocalCacheTileStatus.NotCached tileIsInSrtmDir 
                = NotCached @>

[<Fact>]
let ``When level 0 tile exists in the local cache``() = 
    test <@ decideSrtmTileStatus 
                (SrtmLevel.fromInt 0)
                LocalCacheTileStatus.Cached doNotCallMe 
                = Cached @>

[<Fact>]
let ``When higher level tile is not in the cache``() =
    test <@ decideSrtmTileStatus 
                (SrtmLevel.fromInt 4) 
                LocalCacheTileStatus.NotCached doNotCallMe
                = NotCached @>

[<Fact>]
let ``When higher level tile is marked as not existing``() =
    test <@ decideSrtmTileStatus 
                (SrtmLevel.fromInt 4)
                LocalCacheTileStatus.HigherLevelDoesNotExist doNotCallMe 
                = NotExists @>

[<Fact>]
let ``When higher level tile is exists in the cache``() =
    test <@ decideSrtmTileStatus 
                (SrtmLevel.fromInt 4)
                LocalCacheTileStatus.Cached doNotCallMe 
                = Cached @>
