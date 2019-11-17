module Tests.Srtm.``fetchSrtmHeights tests``

open FsUnit
open Xunit
open Demeton.DemTypes
open Demeton.Srtm.Funcs
open Swensen.Unquote
open TestHelp

let tileSize = 10

[<Fact>]
let ``Returns None if there are no tiles to fetch``() =
    let srtmHeights = fetchSrtmHeights (fun _ -> Ok None) [] 
    test <@ srtmHeights = Ok None @>

[<Fact>]
let ``Returns HeightArray when at least one tile was found``() =
    let tilesToUse = [ srtmTileId 0 1 1 ]

    let (minX, minY) = tileMinCell tileSize tilesToUse.[0]
    let returnSomeHeightArray _ =
        HeightsArray
            (minX, minY, tileSize, tileSize,
             HeightsArrayInitializer1D (fun _ -> DemHeightNone))

    let srtmHeights = 
        fetchSrtmHeights 
            (fun x -> Ok (Some (returnSomeHeightArray x)))
            tilesToUse
            
    test <@ isOk srtmHeights @>
    test <@ Option.isSome (resultValue srtmHeights) @>
   