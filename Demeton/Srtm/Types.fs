module Demeton.Srtm.Types

open Demeton.DemTypes

[<Literal>]
let MaxSrtmLevel = 6

[<StructuredFormatDisplay("{Value}")>]
type SrtmLevel =
    { Value: int }

    static member fromInt i =
        if i < 0 || i > MaxSrtmLevel then
            invalidArg "i" "SRTM level is out of range"
        else
            { Value = i }

let (|Level0|HigherLevel|) (level: SrtmLevel) =
    match level.Value with
    | 0 -> Level0
    | _ -> HigherLevel

[<StructuredFormatDisplay("{Value}")>]
type SrtmLatitude =
    { Value: int }

    static member fromInt i =
        if i < -90 || i > 90 then
            invalidArg "i" "Latitude is out of range"
        else
            { Value = i }

[<StructuredFormatDisplay("{Value}")>]
type SrtmLongitude =
    { Value: int }

    static member fromInt i =
        if i < -179 || i > 180 then
            invalidArg "i" "Longitude is out of range"
        else
            { Value = i }

[<StructuredFormatDisplay("{IdString}")>]
type SrtmTileCoords =
    { Lon: SrtmLongitude
      Lat: SrtmLatitude }

    member this.IdString = $"SrtmTile (%d{this.Lon.Value}/%d{this.Lat.Value})"

/// <summary>
/// Identifies a SRTM tile by its level and coordinates.
/// </summary>
[<StructuredFormatDisplay("{IdString}")>]
type SrtmTileId =
    { Level: SrtmLevel
      TileX: int
      TileY: int }

    member this.IdString = $"%d{this.Level.Value}/%d{this.TileX}/%d{this.TileY}"

/// <summary>
/// In-memory representation of a SRTM tile, identified by its ID and holding
/// its heights array.
/// </summary>
type SrtmTile = SrtmTileId * HeightsArray

type SrtmTileCellCoordsInt = (int * int)
type SrtmTileCellCoordsFloat = (float * float)

/// <summary>
/// A function that reads a SRTM tile.
/// </summary>
type SrtmTileReader = SrtmTileId -> HeightsArrayMaybeResult

type SrtmTileName = string
