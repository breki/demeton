module Tests.Srtm.``Fetching SRTM tiles``.``Finalizing fetch SRTM tile processing``

open Demeton.Srtm.Funcs
open Demeton.Srtm.Fetch
open Demeton.DemTypes

open Xunit
open Swensen.Unquote
open TestHelp

let tileHeights = 
    HeightsArray(1, 2, 3, 4, HeightsArrayInitializer1D (fun _ -> DemHeightNone))

// Returns tile's height array if the final tiles stack has only a single tile
// and the final command stack is empty.
[<Fact>]
let ``Successful fetching of a tile``() =
    let finalState = ([], [ Some (srtmTileId 0 1 2, tileHeights)])

    let result = finalState |> finalizeFetchSrtmTileProcessing
    test <@ result |> isOkValue (Some tileHeights) @>

// Returns None if the final tiles stack has only a single None item for a tile
// and the final command stack is empty.
[<Fact>]
let ``Returns None for non-existing tile``() =
    let finalState = ([], [None])

    let result = finalState |> finalizeFetchSrtmTileProcessing
    test <@ result |> isOkValue None @>

// When the final state contains error indicator, the function returns it as an
// error.
[<Fact>]
let ``Handles the error indicator from the final state``() =
    let errorMessage = "some error"
    let finalState =
        ([ Failure errorMessage ], [ Some (srtmTileId 0 1 2, tileHeights)])

    let result = finalState |> finalizeFetchSrtmTileProcessing
    test <@ result |> isErrorData errorMessage @>

