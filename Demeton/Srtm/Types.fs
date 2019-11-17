module Demeton.Srtm.Types

open Demeton.DemTypes

[<Literal>]
let MaxSrtmLevel = 6

[<StructuredFormatDisplay("{Value}")>]
type SrtmLevel = { Value: int } with 
    static member fromInt i =
        if i < 0 || i > MaxSrtmLevel 
            then invalidArg "i" "SRTM level is out of range"
        else { Value = i }

[<StructuredFormatDisplay("{Value}")>]
type SrtmLatitude = { Value: int } with 
    static member fromInt i =
        if i < -90 || i > 90 then invalidArg "i" "Latitude is out of range"
        else { Value = i }

[<StructuredFormatDisplay("{Value}")>]
type SrtmLongitude = { Value: int } with
    static member fromInt i =
        if i < -179 || i > 180 then invalidArg "i" "Longitude is out of range"
        else { Value = i }

[<StructuredFormatDisplay("{IdString}")>]
type SrtmTileCoords = 
    { Lon: SrtmLongitude; Lat: SrtmLatitude }
    with
    member this.IdString = 
        sprintf "SrtmTile (%d/%d)" this.Lon.Value this.Lat.Value

[<StructuredFormatDisplay("{IdString}")>]
type SrtmTileId = { Level: SrtmLevel; TileX: int; TileY: int }
    with
    member this.IdString = 
        sprintf "%d/%d/%d" this.Level.Value this.TileX this.TileY


type SrtmTileCellCoordsInt = (int * int)
type SrtmTileCellCoordsFloat = (float * float)

type SrtmTileReader = SrtmTileId -> HeightsArrayResult


