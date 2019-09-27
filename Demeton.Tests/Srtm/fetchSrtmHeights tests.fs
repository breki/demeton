module Demeton.Tests.``fetchSrtmHeights tests``

open FsUnit
open Xunit
open Demeton.DemTypes
open Demeton.Srtm.Types
open Demeton.Srtm.Funcs
open Swensen.Unquote
open TestHelp

let returnSomeHeightArray _ =
    HeightsArray(0, 0, 0, 0, HeightsArrayInitializer1D (fun _ -> DemHeightNone))

[<Fact>]
let ``Returns None if there are no tiles to fetch``() =
    let srtmHeights = fetchSrtmHeights [] (fun _ -> Ok None) 
    test <@ srtmHeights = Ok None @>

[<Fact>]
let ``Returns HeightArray when at least one tile was found``() =
    let tilesToUse = [ 
        { Lon = SrtmLongitude.fromInt 1; Lat = SrtmLatitude.fromInt 1 } ]
    let srtmHeights = 
        fetchSrtmHeights 
            tilesToUse
            (fun x -> Ok (Some (returnSomeHeightArray x)))
    test <@ isOk srtmHeights @>
    test <@ Option.isSome (resultValue srtmHeights) @>
   