module Srtm.``SRTM tile tests``

open Demeton.Srtm
open Demeton.Srtm.Types

open FsUnit
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Latitude 0 means north``() =
    { Lon = SrtmLongitude.fromInt 10; Lat = SrtmLatitude.fromInt 0 } 
    |> Tile.tileId 
    |> should equal "N00E010"

[<Fact>]
let ``Latitude -1 means south``() =
    { Lon = SrtmLongitude.fromInt 10; Lat = SrtmLatitude.fromInt -1 } 
    |> Tile.tileId 
    |> should equal "S01E010"

[<Fact>]
let ``Longitude 0 means east``() =
    { Lon = SrtmLongitude.fromInt 0; Lat = SrtmLatitude.fromInt 10 } 
    |> Tile.tileId 
    |> should equal "N10E000"

[<Fact>]
let ``Longitude -1 means west``() =
    { Lon = SrtmLongitude.fromInt -1; Lat = SrtmLatitude.fromInt 10 } 
    |> Tile.tileId 
    |> should equal "N10W001"

[<Fact>]
let ``Can parse north and west tile IDs``() =
    test <@ 
            Tile.parseTileId "N10W001" = 
                { Lon = SrtmLongitude.fromInt -1; 
                Lat = SrtmLatitude.fromInt 10 } 
    @>

[<Fact>]
let ``Can parse south and east tile IDs``() =
    test <@ 
            Tile.parseTileId "S22E080" = 
                { Lon = SrtmLongitude.fromInt 80; 
                Lat = SrtmLatitude.fromInt -22 } 
    @>

[<Fact>]
let ``Calculates global coordinates for a given tile ID``() =
    test <@ 
            Tile.parseTileId "S22E080"
            |> Tile.tileCellMinCoords 3600 = (932400, 244800)
    @>

[<Theory>]
[<InlineData(0., 0., 1, 178.5, 89.5)>]
[<InlineData(1., 1., 1, 179.5, 90.5)>]
[<InlineData(0.5, 0.5, 1, 179., 90.)>]
[<InlineData(0., 0., 3600, 644399.5, 323999.5)>]
[<InlineData(46.557611, 15.6455, 3600, 812006.8996, 380323.3)>]
let ``Calculates fractional global coordinates for given longitude and latitude``
    longitude latitude tileSize expectedGlobalX expectedGlobalY =
    test <@ Tile.longitudeToGlobalX longitude tileSize = expectedGlobalX @>
    test <@ Tile.latitudeToGlobalY latitude tileSize = expectedGlobalY @>
