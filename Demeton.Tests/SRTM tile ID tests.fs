module Demeton.Tests.``SRTM tile ID tests``

open Demeton
open Demeton.SrtmTypes

open FsUnit
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Latitude 0 means north``() =
    { Lon = SrtmLongitude.fromInt 10; Lat = SrtmLatitude.fromInt 0 } 
    |> Srtm.tileId 
    |> should equal "N00E010"

[<Fact>]
let ``Latitude -1 means south``() =
    { Lon = SrtmLongitude.fromInt 10; Lat = SrtmLatitude.fromInt -1 } 
    |> Srtm.tileId 
    |> should equal "S01E010"

[<Fact>]
let ``Longitude 0 means east``() =
    { Lon = SrtmLongitude.fromInt 0; Lat = SrtmLatitude.fromInt 10 } 
    |> Srtm.tileId 
    |> should equal "N10E000"

[<Fact>]
let ``Longitude -1 means west``() =
    { Lon = SrtmLongitude.fromInt -1; Lat = SrtmLatitude.fromInt 10 } 
    |> Srtm.tileId 
    |> should equal "N10W001"

[<Fact>]
let ``Can parse north and west tile IDs``() =
    test <@ 
            Srtm.parseTileId "N10W001" = 
                { Lon = SrtmLongitude.fromInt -1; 
                Lat = SrtmLatitude.fromInt 10 } 
    @>

[<Fact>]
let ``Can parse south and east tile IDs``() =
    test <@ 
            Srtm.parseTileId "S22E080" = 
                { Lon = SrtmLongitude.fromInt 80; 
                Lat = SrtmLatitude.fromInt -22 } 
    @>

[<Fact>]
let ``Calculates global coordinates for a given tile ID``() =
    test <@ 
            Srtm.parseTileId "S22E080"
            |> Srtm.tileCellMinCoords = { X = 932400; Y = 244800 } 
    @>
