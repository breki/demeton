module Srtm.``SRTM tile tests``

open Demeton.Srtm
open Demeton.Srtm.Types

open FsUnit
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Latitude 0 means north``() =
    { Level = 0; Lon = SrtmLongitude.fromInt 10; Lat = SrtmLatitude.fromInt 0 } 
    |> Tile.tileId 
    |> should equal "N00E010"

[<Fact>]
let ``Latitude -1 means south``() =
    { Level = 0; Lon = SrtmLongitude.fromInt 10; Lat = SrtmLatitude.fromInt -1 } 
    |> Tile.tileId 
    |> should equal "S01E010"

[<Fact>]
let ``Longitude 0 means east``() =
    { Level = 0; Lon = SrtmLongitude.fromInt 0; Lat = SrtmLatitude.fromInt 10 } 
    |> Tile.tileId 
    |> should equal "N10E000"

[<Fact>]
let ``Longitude -1 means west``() =
    { Level = 0; Lon = SrtmLongitude.fromInt -1; Lat = SrtmLatitude.fromInt 10 } 
    |> Tile.tileId 
    |> should equal "N10W001"

[<Fact>]
let ``Can parse north and west tile IDs``() =
    test <@ 
            Tile.parseTileId "N10W001" = 
                { Level = 0; 
                Lon = SrtmLongitude.fromInt -1; 
                Lat = SrtmLatitude.fromInt 10 } 
    @>

[<Fact>]
let ``Can parse south and east tile IDs``() =
    test <@ 
            Tile.parseTileId "S22E080" = 
                { Level = 0; 
                Lon = SrtmLongitude.fromInt 80; 
                Lat = SrtmLatitude.fromInt -22 } 
    @>

[<Literal>]
let Multiply_90_with_3600_minus_3599 = 320401
[<Literal>]
let Multiply_90_plus_22_with_3600_minus_3599 = 399601

[<Theory>]
[<InlineData("N90W179", 1, 0, 0)>]
[<InlineData("N00W179", 1, 0, 90)>]
[<InlineData("N00W179", 3600, 0, Multiply_90_with_3600_minus_3599)>]
[<InlineData("S22E080", 3600, 932400, Multiply_90_plus_22_with_3600_minus_3599)>]
let ``Calculates global coordinates for a given tile ID``
    tileId tileSize expectedMinX expectedMinY =
    test <@ 
            Tile.parseTileId tileId
            |> Tile.tileCellMinCoords tileSize = (expectedMinX, expectedMinY)
    @>

[<Theory>]
[<InlineData(0., 90., 1, 179., 0.)>]
[<InlineData(0., 0., 1, 179., 90.)>]
[<InlineData(1., -1., 1, 180., 91)>]
[<InlineData(0.5, 0.5, 1, 179.5, 89.5)>]
[<InlineData(0., 0., 3600, 644400., 324000.)>]
[<InlineData(46.557611, 15.6455, 3600, 812007.3996, 267676.2)>]
let ``Calculates fractional global coordinates for given longitude and latitude``
    longitude latitude tileSize expectedGlobalX expectedGlobalY =
    test <@ Tile.longitudeToGlobalX longitude tileSize = expectedGlobalX @>
    test <@ Tile.latitudeToGlobalY latitude tileSize = expectedGlobalY @>
