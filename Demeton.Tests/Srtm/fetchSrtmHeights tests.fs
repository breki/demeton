module Demeton.Tests.``fetchSrtmHeights tests``

open FsUnit
open Xunit
open Demeton.DemTypes
open Demeton.Srtm.Funcs
open Swensen.Unquote
open TestHelp
open Tests.Srtm.SrtmHelper

let returnSomeHeightArray _ =
    HeightsArray(0, 0, 0, 0, HeightsArrayInitializer1D (fun _ -> DemHeightNone))

[<Fact>]
let ``Returns None if there are no tiles to fetch``() =
    let srtmHeights = fetchSrtmHeights (fun _ -> Ok None) [] 
    test <@ srtmHeights = Ok None @>

[<Fact>]
let ``Returns HeightArray when at least one tile was found``() =
    let tilesToUse = [ srtmTileCoords 0 1 1 ]
    let srtmHeights = 
        fetchSrtmHeights 
            (fun x -> Ok (Some (returnSomeHeightArray x)))
            tilesToUse
            
    test <@ isOk srtmHeights @>
    test <@ Option.isSome (resultValue srtmHeights) @>
   