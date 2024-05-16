module Tests.Srtm.SrtmHelper

open Demeton.Dem.Types

let srtmTileCoords lon lat =
    { Lon = SrtmLongitude.fromInt lon
      Lat = SrtmLatitude.fromInt lat }
