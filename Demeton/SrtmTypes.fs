module Demeton.SrtmTypes

open Demeton.DemTypes

type SrtmLatitude = { Value: int } with 
    static member fromInt i =
        if i < -90 || i > 90 then invalidArg "i" "Latitude is out of range"
        else { Value = i }

type SrtmLongitude = { Value: int } with
    static member fromInt i =
        if i < -179 || i > 180 then invalidArg "i" "Longitude is out of range"
        else { Value = i }

[<StructuredFormatDisplay("SrtmTile ({Lon}, {Lat})")>]
type SrtmTileCoords = { Lon: SrtmLongitude; Lat: SrtmLatitude }

type SrtmTileHgtFile = SrtmTileHgtFile of SrtmTileCoords * string

type FetchSrtmTiles = SrtmTileCoords seq -> SrtmTileHgtFile seq

type ReadSrtmTile = SrtmTileHgtFile -> HeightsArray


