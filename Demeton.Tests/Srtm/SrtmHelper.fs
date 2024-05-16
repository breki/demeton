module Tests.Srtm.SrtmHelper

open Demeton.Dem.Types

let srtmTileCoords lon lat =
    { Lon = DemLongitude.fromInt lon
      Lat = DemLatitude.fromInt lat }
