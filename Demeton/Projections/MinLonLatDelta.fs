/// <summary>
/// Provides an algorithm to determine the minimum delta of longitudes and
/// latitudes between two neighboring points of a raster. This algorithm is then
/// used to determine which SRTM DEM level is needed and what is the maximum
/// DPI that can actually be provided when generating the shaded raster.
/// </summary>
/// <remarks>The algoritm uses
/// simulated annealing to "walk" along the raster, looking for a point with
/// the minimum longitude/latitude delta.
/// </remarks>
module Demeton.Projections.MinLonLatDelta

open Demeton.Srtm.Types
open Demeton.Projections.Common
open SimAnn
open System

type LonLatDelta = float

/// <summary>
/// Type containing the state for the simulated annealing. Basically just a 
/// point on the raster.
/// </summary>
type MinLonLatDeltaState = Raster.Point

let private rasterXYToLonLat
    (inverse: InvertFunc) (rasterX, rasterY) scaleFactor =
    inverse 
        ((float rasterX) / scaleFactor)
        -((float rasterY) / scaleFactor)

/// <summary>
/// Calculates the minimum longitude/latitude delta for the given raster point.
/// </summary>
let calculateLonLatDeltaOfPoint
    (inverse: InvertFunc) 
    rasterSamplePointX rasterSamplePointY
    scaleFactor
    =
    let lonlat0Maybe = 
        rasterXYToLonLat 
            inverse (rasterSamplePointX, rasterSamplePointY) scaleFactor
    let lonlat1Maybe = 
        rasterXYToLonLat
            inverse (rasterSamplePointX + 1, rasterSamplePointY + 1) scaleFactor

    match (lonlat0Maybe, lonlat1Maybe) with
    | (Some (lon0, lat0), Some (lon1, lat1)) ->
        min (abs (lon1-lon0)) (abs (lat1-lat0))
    | _ -> Double.MaxValue

/// <summary>
/// Energy function for simulated annealing algorithm used to find the minimum
/// delta longitude/latitude. Basically, it just calls 
/// <see cref="calculateLonLatDeltaOfPoint" /> function.
/// </summary>
let srtmMinCellEnergy (inverse: InvertFunc) scaleFactor
    : EnergyFunc<MinLonLatDeltaState> =
    fun (rasterX, rasterY) ->
        calculateLonLatDeltaOfPoint inverse rasterX rasterY scaleFactor

let private srtmMinCellDeltaNeighbor (rasterRect: Raster.Rect) : 
    NeighborFunc<MinLonLatDeltaState> = 
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
let minLonLatDelta
    (rasterRect: Raster.Rect)
    (inverse: InvertFunc) 
    scaleFactor: LonLatDelta =
    let initialState = (
        rasterRect.MinX + rasterRect.Width / 2,
        rasterRect.MinY + rasterRect.Height / 2 )

    let finalMinDeltaPoint = 
        simulatedAnnealing 
            (annealingScheduleExponential 100. 0.85) 
            initialState 
            (srtmMinCellDeltaNeighbor rasterRect)
            (srtmMinCellEnergy inverse scaleFactor)
            kirkpatrickAcceptanceProbability
            1000 
    
    srtmMinCellEnergy inverse scaleFactor finalMinDeltaPoint

/// <summary>
/// Calculates the required SRTM level for a given lon/lat delta.
/// </summary>
let lonLatDeltaToSrtmLevel tileSize (lonLatDelta: LonLatDelta): SrtmLevel = 
    let lonLatDeltaDeg = Demeton.Geometry.Common.radToDeg lonLatDelta

    // size of an SRTM cell (in degrees)
    let srtmCellSizeDeg = 1.0 / (float tileSize)

    // resolution is in terms of SRTM cells
    let resolutionNeeded = lonLatDeltaDeg / srtmCellSizeDeg

    min MaxSrtmLevel (max 0 (Math.Log2 resolutionNeeded |> int)) 
    |> SrtmLevel.fromInt
