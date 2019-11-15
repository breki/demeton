module Tests.Srtm.``Fetching SRTM tiles``.``Finalizing fetch SRTM tile processing``

open Demeton.Srtm.Fetch
open Demeton.DemTypes

open Xunit
open Swensen.Unquote
open TestHelp
open Tests.Srtm.SrtmHelper

let cacheDir = "somecache"

let tileHeights = 
    HeightsArray(1, 2, 3, 4, HeightsArrayInitializer1D (fun _ -> DemHeightNone))

let readPngTileSuccessfully _ _ = Ok tileHeights
let readingPngTileFails errorMessage _ _ = Error errorMessage

// Reads PNG tile if the final tiles stack has only a single tile and the final 
// command stack is empty.
[<Fact>]
let ``Successful fetching of a tile``() =
    let finalState = ([], [ Some (srtmTileCoords 0 1 2)])

    let result = 
        finalState
        |> finalizeFetchSrtmTileProcessing cacheDir readPngTileSuccessfully
    test <@ result |> isOkValue (Some tileHeights) @>

// When reading of PNG tile fails, the error is forwarded.
[<Fact>]
let ``Handles the error when reading the PNG tile``() =
    let errorMessage = "some error"
    let finalState = ([], [ Some (srtmTileCoords 0 1 2)])

    let result = 
        finalState
        |> finalizeFetchSrtmTileProcessing 
            cacheDir (readingPngTileFails errorMessage)
    test <@ result |> isErrorData errorMessage @>

// Returns None if the final tiles stack has only a single None item for a tile
// and the final command stack is empty.
[<Fact>]
let ``Returns None for non-existing tile``() =
    let finalState = ([], [None])

    let result = 
        finalState
        |> finalizeFetchSrtmTileProcessing cacheDir readPngTileSuccessfully
    test <@ result |> isOkValue None @>

// When the final state contains error indicator, the function returns it as an
// error.
[<Fact>]
let ``Handles the error indicator from the final state``() =
    let errorMessage = "some error"
    let finalState = ([ Failure errorMessage ], [ Some (srtmTileCoords 0 1 2)])

    let result = 
        finalState
        |> finalizeFetchSrtmTileProcessing cacheDir 
            (fun _ -> invalidOp "should not be called")
    test <@ result |> isErrorData errorMessage @>

