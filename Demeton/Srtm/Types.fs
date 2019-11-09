module Demeton.Srtm.Types

open Demeton.DemTypes

[<StructuredFormatDisplay("{Value}")>]
type SrtmLatitude = { Value: int } with 
    static member fromInt i =
        if i < -90 || i > 90 then invalidArg "i" "Latitude is out of range"
        else { Value = i }

[<StructuredFormatDisplay("{Value}")>]
[<StructuralComparison>]
type SrtmLongitude = { Value: int } with
    static member fromInt i =
        if i < -179 || i > 180 then invalidArg "i" "Longitude is out of range"
        else { Value = i }

[<StructuredFormatDisplay("SrtmTile ({Level}/{Lon}/{Lat})")>]
[<Struct>]
type SrtmTileCoords = { Level: int; Lon: SrtmLongitude; Lat: SrtmLatitude }

type SrtmTileFile = { TileCoords: SrtmTileCoords; FileName: string }

type SrtmTileReader = SrtmTileCoords -> HeightsArrayResult


