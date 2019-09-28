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

[<Fact>]
let ``Calculates fractional global coordinates for given longitude and latitude``() =
    let latitude = 46.557611
    let longitude = 15.6455
    let tileSize = 3600
    
    test <@ Tile.longitudeToGlobalX longitude tileSize 
        = (179. + longitude) * float tileSize @>
    test <@ Tile.latitudeToGlobalY latitude tileSize 
        = (90. + latitude) * float tileSize @>
