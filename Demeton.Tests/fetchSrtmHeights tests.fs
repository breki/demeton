module Demeton.Tests.``fetchSrtmHeights tests``

open FsUnit
open Xunit
open Demeton.DemTypes
open Demeton.SrtmTypes
open Swensen.Unquote

let returnSomeHeightArray _ =
    HeightArray({ X = 0; Y = 0}, 0, 0, (fun _ -> None))

let fetchSomeSrtmTiles tilesCoords =
    tilesCoords |> Seq.map (fun tc -> SrtmTileHgtFile (tc, "sometile") );

[<Fact>]
let ``Returns None if there are no tiles to fetch``() =
    let srtmHeights = 
        Demeton.Srtm.fetchSrtmHeights 
            [] 
            (fun _ -> Seq.empty<SrtmTileHgtFile>) 
            returnSomeHeightArray 
    srtmHeights |> should equal None
    test <@ (srtmHeights |> Option.isNone) = true @>

[<Fact>]
let ``Returns HeightArray when at least one tile was found``() =
    let tilesToUse = [ { Lon = 1; Lat = 1 } ]
    let srtmHeights = 
        Demeton.Srtm.fetchSrtmHeights 
            tilesToUse
            fetchSomeSrtmTiles
            returnSomeHeightArray
    srtmHeights |> should be ofExactType<HeightArray option>
    test <@ (srtmHeights |> Option.isSome) = true @>
   