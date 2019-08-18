module Demeton.SrtmTypes

open Demeton.DemTypes

[<StructuredFormatDisplay("SrtmTile ({Lon}, {Lat})")>]
type SrtmTileCoords = { Lon: int; Lat: int }

type SrtmTileHgtFile = SrtmTileHgtFile of SrtmTileCoords * string

type FetchSrtmTiles = SrtmTileCoords seq -> SrtmTileHgtFile seq

type ReadSrtmTile = SrtmTileHgtFile -> DemData


