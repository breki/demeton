module Demeton

type Bounds = { 
    MinLon: double
    MinLat: double 
    MaxLon: double 
    MaxLat: double
    }

[<StructuredFormatDisplay("SrtmTile ({Lon}, {Lat})")>]
type SrtmTile = {
    Lon: int
    Lat: int
    }

let boundsToTiles (bounds: Bounds): SrtmTile list =
    [ { 
        Lon = (int)(floor bounds.MinLon); 
        Lat = (int)(floor bounds.MinLat)} ]
