module Demeton.Tests.``Tile ID tests``

open Demeton

open FsUnit
open Xunit

[<Fact>]
let ``Latitude 0 means north``() =
    { Lon = 10; Lat = 0 } |> tileId 
    |> should equal "N00E010"

[<Fact>]
let ``Latitude -1 means south``() =
    { Lon = 10; Lat = -1 } |> tileId 
    |> should equal "S01E010"

[<Fact>]
let ``Longitude 0 means east``() =
    { Lon = 0; Lat = 10 } |> tileId 
    |> should equal "N10E000"

[<Fact>]
let ``Longitude -1 means west``() =
    { Lon = -1; Lat = 10 } |> tileId 
    |> should equal "N10W001"
