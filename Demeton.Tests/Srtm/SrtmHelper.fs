module Tests.Srtm.SrtmHelper

open Demeton.Srtm.Types

let srtmTileCoords level lon lat =
    { Level = SrtmLevel.fromInt level; 
    Lon = SrtmLongitude.fromInt lon; 
    Lat = SrtmLatitude.fromInt lat } 
