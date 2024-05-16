module Tests.Srtm.``Fetching SRTM tiles``.``Checking local cache status``

open Demeton.Srtm.Fetch
open Demeton.Dem.Types

open Xunit
open Swensen.Unquote

let doNotCallMe = Lazy<bool>(fun () -> invalidOp "should not be called")

let pngNotInCache = false
let pngInCache = true

let noneFileInCache = Lazy<bool>(fun () -> true)
let noneFileNotInCache = Lazy<bool>(fun () -> false)

[<Fact>]
let ``If PNG file of level > 0 is not in the cache, and '.none' is also not, it is marked as not cached``
    ()
    =
    test
        <@
            determineLocalCacheTileStatus
                (DemLevel.fromInt 4)
                pngNotInCache
                noneFileNotInCache = LocalCacheTileStatus.NotCached
        @>

[<Fact>]
let ``If PNG file of level > 0 is not in the cache, and '.none' exists, it is marked as not existing``
    ()
    =
    test
        <@
            determineLocalCacheTileStatus
                (DemLevel.fromInt 4)
                pngNotInCache
                noneFileInCache = LocalCacheTileStatus.HigherLevelDoesNotExist
        @>

[<Fact>]
let ``If PNG file of level > 0 is in the cache, it is marked as cached`` () =
    test
        <@
            determineLocalCacheTileStatus
                (DemLevel.fromInt 4)
                pngInCache
                doNotCallMe = LocalCacheTileStatus.Cached
        @>

[<Fact>]
let ``If PNG file of level 0 is not in the cache, it is marked as not existing``
    ()
    =
    test
        <@
            determineLocalCacheTileStatus
                (DemLevel.fromInt 0)
                pngNotInCache
                doNotCallMe = LocalCacheTileStatus.NotCached
        @>

[<Fact>]
let ``If PNG file of level 0 is in the cache, it is marked as cached`` () =
    test
        <@
            determineLocalCacheTileStatus
                (DemLevel.fromInt 0)
                pngInCache
                doNotCallMe = LocalCacheTileStatus.Cached
        @>
