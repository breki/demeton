module Tests.Srtm.SrtmHelper

open Demeton.Srtm.Types

let srtmTileCoords lon lat =
    { Lon = SrtmLongitude.fromInt lon; 
    Lat = SrtmLatitude.fromInt lat } 
