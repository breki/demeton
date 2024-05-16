module Demeton.Srtm.Types

open Demeton.Dem.Types


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
/// In-memory representation of a SRTM tile, identified by its ID and holding
/// its heights array.
/// </summary>
type SrtmTile = SrtmTileId * HeightsArray

type SrtmTileCellCoordsInt = int * int
type SrtmTileCellCoordsFloat = float * float

/// <summary>
/// A function that reads a SRTM tile.
/// </summary>
type SrtmTileReader = SrtmTileId -> HeightsArrayMaybeResult

type SrtmTileName = string
