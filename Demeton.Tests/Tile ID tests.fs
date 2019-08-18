module Demeton.Tests.``Tile ID tests``

open Demeton
open Demeton.SrtmTypes

open FsUnit
open Xunit

[<Fact>]
let ``Latitude 0 means north``() =
    { Lon = 10; Lat = 0 } |> Srtm.tileId 
    |> should equal "N00E010"

[<Fact>]
let ``Latitude -1 means south``() =
    { Lon = 10; Lat = -1 } |> Srtm.tileId 
    |> should equal "S01E010"

[<Fact>]
let ``Longitude 0 means east``() =
    { Lon = 0; Lat = 10 } |> Srtm.tileId 
    |> should equal "N10E000"

[<Fact>]
let ``Longitude -1 means west``() =
    { Lon = -1; Lat = 10 } |> Srtm.tileId 
    |> should equal "N10W001"
