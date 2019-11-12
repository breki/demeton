module Tests.Srtm.``Fetching SRTM tiles``.``Checking complete tile status``

open Demeton.Srtm.Fetch

open Xunit
open Swensen.Unquote
open Tests.Srtm.SrtmHelper

let doNotCallMe: SrtmDirTileStatusChecker = 
    (fun _ _ -> invalidOp "should not be called")

[<Fact>]
let ``When level 0 tile neither exists in the cache nor in the store``() =
    let tile = srtmTileCoords 0 10 20
    test <@ checkSrtmTileStatus 
                (fun _ _ -> SrtmDirTileStatus.DoesNotExist)
                tile LocalCacheTileStatus.NotCached = NotExists @>

[<Fact>]
let ``When level 0 tile is not cached but it exists in SRTM dir``() = 
    let tile = srtmTileCoords 0 10 20
    test <@ checkSrtmTileStatus 
                (fun _ _ -> SrtmDirTileStatus.Exists)
                tile LocalCacheTileStatus.NotCached = NotCached @>

[<Fact>]
let ``When level 0 tile exists in the local cache``() = 
    let tile = srtmTileCoords 0 10 20
    test <@ checkSrtmTileStatus 
                doNotCallMe
                tile LocalCacheTileStatus.Cached = Cached @>

[<Fact>]
let ``When higher level tile is not in the cache``() =
    let tile = srtmTileCoords 4 10 20
    test <@ checkSrtmTileStatus 
                doNotCallMe
                tile LocalCacheTileStatus.NotCached = NotCached @>

[<Fact>]
let ``When higher level tile is marked as not existing``() =
    let tile = srtmTileCoords 4 10 20
    test <@ checkSrtmTileStatus 
                doNotCallMe
                tile LocalCacheTileStatus.HigherLevelDoesNotExist = NotExists @>

[<Fact>]
let ``When higher level tile is exists in the cache``() =
    let tile = srtmTileCoords 4 10 20
    test <@ checkSrtmTileStatus 
                doNotCallMe
                tile LocalCacheTileStatus.Cached = Cached @>
