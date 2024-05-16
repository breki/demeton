module Tests.Srtm.``fetchSrtmHeights tests``

open FsUnit
open Xunit
open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Swensen.Unquote
open TestHelp

let tileSize = 10

[<Fact>]
let ``Returns None if there are no tiles to fetch`` () =
    let srtmHeights = fetchDemHeights (fun _ -> Ok None) []
    test <@ srtmHeights = Ok None @>

[<Fact>]
let ``Returns HeightArray when at least one tile was found`` () =
    let tilesToUse = [ demTileId 0 1 1 ]

    let (minX, minY) = tileMinCell tileSize tilesToUse.[0]

    let returnSomeHeightArray _ =
        HeightsArray(
            minX,
            minY,
            tileSize,
            tileSize,
            HeightsArrayInitializer1D(fun _ -> DemHeightNone)
        )

    let srtmHeights =
        fetchDemHeights (returnSomeHeightArray >> Some >> Ok) tilesToUse

    test <@ isOk srtmHeights @>
    test <@ Option.isSome (resultValue srtmHeights) @>
