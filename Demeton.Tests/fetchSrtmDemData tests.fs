module Demeton.Tests.``fetchSrtmDemData tests``

open Demeton

open FsUnit
open Xunit

let returnSomeDemData _ =
    DemData(0, 0)

let fetchSomeSrtmTiles tilesCoords =
    tilesCoords |> Seq.map (fun tc -> SrtmTileHgtFile (tc, "sometile") );

[<Fact>]
let ``Returns None if there are no tiles to fetch``() =
    fetchSrtmDemData 
        [] 
        (fun _ -> Seq.empty<SrtmTileHgtFile>) 
        returnSomeDemData 
    |> should equal None

[<Fact>]
let ``Returns DemData when at least one tile was found``() =
    let tilesToUse = [ { Lon = 1; Lat = 1 } ]
    let demData = 
        fetchSrtmDemData 
            tilesToUse
            fetchSomeSrtmTiles
            returnSomeDemData
    demData |> should be ofExactType<DemData>
    
