/// <summary>
/// Provides an algorithm to determine the minimum delta of longitudes and
/// latitudes between two neighboring points of a raster. This algorithm is then
/// used to determine which SRTM DEM level is needed and what is the maximum
/// DPI that can actually be provided when generating the shaded raster.
/// </summary>
/// <remarks>The algorithm uses
/// simulated annealing to "walk" along the raster, looking for a point with
/// the minimum longitude/latitude delta.
/// </remarks>
module Demeton.Projections.MinLonLatDelta

open Demeton.Dem.Types
open Demeton.Projections.Common
open SimAnn
open System

type LonLatDelta = float

/// <summary>
/// Type containing the state for the simulated annealing. Basically just a
/// point on the raster.
/// </summary>
type MinLonLatDeltaState = Raster.Point

let private rasterXYToLonLat (inverse: InvertFunc) (rasterX, rasterY) =
    inverse (float rasterX) -(float rasterY)

/// <summary>
/// Calculates the minimum longitude/latitude delta for the given raster point.
/// </summary>
let calculateLonLatDeltaOfPoint
    (inverse: InvertFunc)
    rasterSamplePointX
    rasterSamplePointY
    =
    let lonlat0Maybe =
        rasterXYToLonLat inverse (rasterSamplePointX, rasterSamplePointY)

    let lonlat1Maybe =
        rasterXYToLonLat
            inverse
            (rasterSamplePointX + 1, rasterSamplePointY + 1)

    match (lonlat0Maybe, lonlat1Maybe) with
    | Some(lon0, lat0), Some(lon1, lat1) ->
        min (abs (lon1 - lon0)) (abs (lat1 - lat0))
    | _ -> Double.MaxValue

/// <summary>
/// Energy function for simulated annealing algorithm used to find the minimum
/// delta longitude/latitude. Basically, it just calls
/// <see cref="calculateLonLatDeltaOfPoint" /> function.
/// </summary>
let srtmMinCellEnergy (inverse: InvertFunc) : EnergyFunc<MinLonLatDeltaState> =
    fun (rasterX, rasterY) ->
        calculateLonLatDeltaOfPoint inverse rasterX rasterY

let private srtmMinCellDeltaNeighbor
    (rasterRect: Raster.Rect)
    : NeighborFunc<MinLonLatDeltaState> =
    fun ((rasterX, rasterY): MinLonLatDeltaState) (rnd: Random) ->

        let rasterDx = rnd.Next(0, 10 + 1) - 5
        let rasterDy = rnd.Next(0, 10 + 1) - 5

        let newRasterX = rasterX + rasterDx
        let newRasterY = rasterY + rasterDy

        let newRasterX = min (max newRasterX rasterRect.MinX) rasterRect.MaxX
        let newRasterY = min (max newRasterY rasterRect.MinY) rasterRect.MaxY

        (newRasterX, newRasterY)

/// <summary>
/// Calculates the minimum longitude/latitude delta for the given raster.
/// Currently it only supports Mercator projection, but this will be fixed
/// once we start supporting custom map projections.
/// </summary>
let minLonLatDelta (rasterRect: Raster.Rect) (inverse: InvertFunc) : float =
    let initialState =
        (rasterRect.MinX + rasterRect.Width / 2,
         rasterRect.MinY + rasterRect.Height / 2)

    let finalMinDeltaPoint =
        simulatedAnnealing
            (annealingScheduleExponential 100. 0.85)
            initialState
            (srtmMinCellDeltaNeighbor rasterRect)
            (srtmMinCellEnergy inverse)
            kirkpatrickAcceptanceProbability
            1000

    srtmMinCellEnergy inverse finalMinDeltaPoint

/// <summary>
/// Calculates the required DEM level for a given lon/lat delta.
/// </summary>
let lonLatDeltaToDemLevel tileSize (lonLatDelta: LonLatDelta) : DemLevel =
    let lonLatDeltaDeg = Demeton.Geometry.Common.radToDeg lonLatDelta

    // size of an DEM cell (in degrees)
    let demCellSizeDeg = 1.0 / (float tileSize)

    // resolution is in terms of DEM cells
    let resolutionNeeded = lonLatDeltaDeg / demCellSizeDeg

    min MaxDemLevel (max 0 (Math.Log2 resolutionNeeded |> int))
    |> DemLevel.fromInt
