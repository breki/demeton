module Demeton.Tests.``fetchSrtmDemData tests``

open FsUnit
open Xunit
open Demeton.DemTypes
open Demeton.SrtmTypes
open Swensen.Unquote

let returnSomeDemData _ =
    DemData(0, 0, 0, 0)

let fetchSomeSrtmTiles tilesCoords =
    tilesCoords |> Seq.map (fun tc -> SrtmTileHgtFile (tc, "sometile") );

[<Fact>]
let ``Returns None if there are no tiles to fetch``() =
    let demData = 
        Demeton.Srtm.fetchSrtmDemData 
            [] 
            (fun _ -> Seq.empty<SrtmTileHgtFile>) 
            returnSomeDemData 
    demData |> should equal None
    test <@ (demData |> Option.isNone) = true @>

[<Fact>]
let ``Returns DemData when at least one tile was found``() =
    let tilesToUse = [ { Lon = 1; Lat = 1 } ]
    let demData = 
        Demeton.Srtm.fetchSrtmDemData 
            tilesToUse
            fetchSomeSrtmTiles
            returnSomeDemData
    demData |> should be ofExactType<DemData option>
    test <@ (demData |> Option.isSome) = true @>
   