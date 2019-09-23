module Demeton.Tests.``fetchSrtmHeights tests``

open FsUnit
open Xunit
open Demeton.DemTypes
open Demeton.SrtmTypes
open Swensen.Unquote

let returnSomeHeightArray _ =
    HeightsArray(0, 0, 0, 0, (fun _ -> None))

[<Fact>]
let ``Returns None if there are no tiles to fetch``() =
    let srtmHeights = 
        Demeton.Srtm.fetchSrtmHeights 
            [] 
            (fun _ -> None) 
    srtmHeights |> should equal None
    test <@ (srtmHeights |> Option.isNone) = true @>

[<Fact>]
let ``Returns HeightArray when at least one tile was found``() =
    let tilesToUse = [ 
        { Lon = SrtmLongitude.fromInt 1; Lat = SrtmLatitude.fromInt 1 } ]
    let srtmHeights = 
        Demeton.Srtm.fetchSrtmHeights 
            tilesToUse
            (fun x -> Some (returnSomeHeightArray x))
    srtmHeights |> should be ofExactType<HeightsArray option>
    test <@ (srtmHeights |> Option.isSome) = true @>
   