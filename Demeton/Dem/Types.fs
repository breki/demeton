module Demeton.Dem.Types

open System
open Raster

type DemHeight = int16

type DemTileX = int
type DemTileY = int
type DemTileFractionalX = float
type DemTileFractionalY = float

type DemCellX = int
type DemCellY = int
type DemCellFractionalX = float
type DemCellFractionalY = float

[<Literal>]
let DemHeightNone = Int16.MinValue

let interpolateHeight h1 h2 h3 h4 (dx: float) (dy: float) =
    if
        h1 = DemHeightNone
        || h2 = DemHeightNone
        || h3 = DemHeightNone
        || h4 = DemHeightNone
    then
        None
    else
        let hh1 = float (h2 - h1) * dx + float h1
        let hh2 = float (h4 - h3) * dx + float h3
        let height = float (hh2 - hh1) * dy + hh1
        Some height

let inline DemHeight x = int16 x

type DemLocalCellCoords = DemCellX * DemCellY
type DemGlobalCellCoords = DemCellX * DemCellY

type HeightCell =
    { Coords: DemGlobalCellCoords
      Height: DemHeight }

type HeightsArrayInitializer =
    | EmptyHeightsArray
    | HeightsArrayDirectImport of DemHeight[]
    | HeightsArrayInitializer1D of (int -> DemHeight)
    | HeightsArrayInitializer2D of (DemGlobalCellCoords -> DemHeight)
    | HeightsArrayCustomInitializer of (DemHeight[] -> unit)


/// <summary>
/// Represents a two-dimensional array of heights in a digital elevation model
/// (DEM).
/// </summary>
/// <param name="minX">The minimum x-coordinate of the array.</param>
/// <param name="minY">The minimum y-coordinate of the array.</param>
/// <param name="width">The width of the array.</param>
/// <param name="height">The height of the array.</param>
/// <param name="initializer">
/// The initializer used to populate the array.</param>
/// <remarks>
/// The HeightsArray type provides methods for accessing and manipulating
/// the heights in the array.
/// It also provides methods for interpolating heights at fractional
/// cell coordinates.
/// </remarks>
type HeightsArray
    (
        minX: int,
        minY: int,
        width: int,
        height: int,
        initializer: HeightsArrayInitializer
    ) =

    let cells =
        let arraySize = width * height

        match initializer with
        | EmptyHeightsArray ->
            Array.init<DemHeight> arraySize (fun _ -> DemHeightNone)
        | HeightsArrayDirectImport arrayToImport ->
            if arrayToImport.Length <> arraySize then
                invalidOp "The imported heights array is of incompatible size."

            arrayToImport
        | HeightsArrayInitializer2D initializer2D ->
            let initializerFuncToUse =
                fun index ->
                    let x = index % width
                    let y = index / width
                    initializer2D (minX + x, minY + y)

            Array.init<DemHeight> arraySize initializerFuncToUse
        | HeightsArrayInitializer1D initializer1D ->
            Array.init<DemHeight> arraySize initializer1D
        | HeightsArrayCustomInitializer customInitializer ->
            let cellsToInitialize = Array.zeroCreate<DemHeight> arraySize
            customInitializer cellsToInitialize
            cellsToInitialize


    member this.MinX = minX
    member this.MinY = minY
    member this.Width = width
    member this.Height = height
    member this.MaxX = minX + width - 1
    member this.MaxY = minY + height - 1
    member this.Cells = cells

    member this.heightAt((x, y): DemGlobalCellCoords) : DemHeight =

#if DEBUG
        match (x, y) with
        | _ when x < this.MinX || x > this.MaxX ->
            raise <| ArgumentOutOfRangeException("x")
        | _ when y < this.MinY || y > this.MaxY ->
            raise <| ArgumentOutOfRangeException("y")
        | _ -> ()
#endif

        let index = (y - this.MinY) * width + x - this.MinX

        this.Cells.[index]

    member this.heightAtLocal((x, y): Point) : DemHeight =
#if DEBUG
        match (x, y) with
        | _ when x < 0 || x > width -> raise <| ArgumentOutOfRangeException("x")
        | _ when y < 0 || y > height ->
            raise <| ArgumentOutOfRangeException("y")
        | _ -> ()
#endif

        let index = y * width + x

        this.Cells.[index]


    member this.interpolateHeightAt((x, y): float * float) : float option =
        let fractionOf value = value - floor value

        let x1 = int (floor x)
        let x2 = int (ceil x)
        let y1 = int (floor y)
        let y2 = int (ceil y)

        let h1 = this.heightAt (x1, y1)
        let h2 = this.heightAt (x2, y1)
        let h3 = this.heightAt (x1, y2)
        let h4 = this.heightAt (x2, y2)
        interpolateHeight h1 h2 h3 h4 (fractionOf x) (fractionOf y)

    member this.setHeightAt ((x, y): DemGlobalCellCoords) (height: DemHeight) =
        let index = (y - this.MinY) * width + x - this.MinX
        this.Cells.[index] <- height

    member this.setHeightAtLocal ((x, y): Point) (height: DemHeight) =
        let index = y * width + x
        this.Cells.[index] <- height

/// <summary>
/// A result of an operation that returns <see cref="HeightsArray" />.
/// </summary>
type HeightsArrayResult = Result<HeightsArray, string>

/// <summary>
/// A result of an operation that can return an optional
/// <see cref="HeightsArray" />.
/// </summary>
type HeightsArrayMaybeResult = Result<HeightsArray option, string>

[<Literal>]
let MaxDemLevel = 6

[<StructuredFormatDisplay("{Value}")>]
type DemLevel =
    { Value: int }

    static member fromInt i =
        if i < 0 || i > MaxDemLevel then
            invalidArg "i" "DEM level is out of range"
        else
            { Value = i }

let (|Level0|HigherLevel|) (level: DemLevel) =
    match level.Value with
    | 0 -> Level0
    | _ -> HigherLevel


/// <summary>
/// Represents a digital elevation model tile identifier in the
/// SRTM (Shuttle Radar Topography Mission) format.
/// </summary>
/// <remarks>
/// A SRTM tile is identified by its level and coordinates (TileX and TileY).
/// The tile level is an extension of the original SRTM format, whose tile
/// files represent level 0, while the higher levels are generated by
/// merging four adjacent tiles of the lower level and downsampling the
/// height data.
/// </remarks>
[<StructuredFormatDisplay("{IdString}")>]
type DemTileId =
    {
        /// <summary>
        /// Represents the level of the DEM tile. This is of type `DemLevel`.
        /// </summary>
        Level: DemLevel

        /// <summary>
        /// Represents the X coordinate of the DEM tile.
        /// This is an integer value which is positive for tiles east of the
        /// Greenwich meridian and negative for tiles west of it.
        /// </summary>
        TileX: DemTileX

        /// <summary>
        /// Represents the Y coordinate of the DEM tile.
        /// This is an integer value which is positive for tile south of the
        /// Equator and negative for tiles north of it.
        /// </summary>
        TileY: DemTileY
    }

    member this.IdString = $"%d{this.Level.Value}/%d{this.TileX}/%d{this.TileY}"

    member this.FormatLat2Lon3 =
        let lonSign, latSign = this.LonLatCardinalDirectionsSigns

        $"%c{latSign}%02d{abs this.TileY}%c{lonSign}%03d{abs this.TileX}"

    member this.FormatLat3Lon3 =
        let lonSign, latSign = this.LonLatCardinalDirectionsSigns

        $"%c{latSign}%03d{abs this.TileY}%c{lonSign}%03d{abs this.TileX}"

    member this.LonLatCardinalDirectionsSigns =
        let latSign =
            match this.TileY with
            | y when y >= 0 -> 'N'
            | _ -> 'S'

        let lonSign =
            match this.TileX with
            | x when x >= 0 -> 'E'
            | _ -> 'W'

        lonSign, latSign


/// <summary>
/// In-memory representation of a DEM tile, identified by its ID and holding
/// its heights array.
/// </summary>
type DemTile = DemTileId * HeightsArray


[<StructuredFormatDisplay("{Value}")>]
type DemLatitude =
    { Value: int }

    static member fromInt i =
        if i < -90 || i > 90 then
            invalidArg "i" "Latitude is out of range"
        else
            { Value = i }

[<StructuredFormatDisplay("{Value}")>]
type DemLongitude =
    { Value: int }

    static member fromInt i =
        if i < -180 || i > 180 then
            invalidArg "i" "Longitude is out of range"
        else
            { Value = i }

[<StructuredFormatDisplay("{IdString}")>]
type DemTileCoords =
    { Lon: DemLongitude
      Lat: DemLatitude }

    member this.IdString = $"DemTile (%d{this.Lon.Value}/%d{this.Lat.Value})"

/// <summary>
/// A function that reads a DEM tile.
/// </summary>
type DemTileReader = DemTileId -> HeightsArrayMaybeResult

type DemTileName = string
