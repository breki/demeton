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
open Demeton.Projections
open SimAnn
open System

type LonLatDelta = float

/// <summary>
/// Type containing the state for the simulated annealing. Basically just a 
/// point on the raster.
/// </summary>
type MinLonLatDeltaState = Raster.Point

let private rasterXYToLonLat (rasterX, rasterY) scaleFactor =
    WebMercator.inverse 
        ((float rasterX) / scaleFactor)
        -((float rasterY) / scaleFactor)

/// <summary>
/// Calculates the minimum longitude/latitude delta for the given raster point.
/// </summary>
let calculateLonLatDeltaOfPoint
    rasterSamplePointX rasterSamplePointY
    scaleFactor
    =
    let lonlat0Maybe = 
        rasterXYToLonLat 
            (rasterSamplePointX, rasterSamplePointY) scaleFactor
    let lonlat1Maybe = 
        rasterXYToLonLat
            (rasterSamplePointX + 1, rasterSamplePointY + 1) scaleFactor

    match (lonlat0Maybe, lonlat1Maybe) with
    | (Some (lon0, lat0), Some (lon1, lat1)) ->
        min (abs (lon1-lon0)) (abs (lat1-lat0))
    | _ -> Double.MaxValue

/// <summary>
/// Energy function for simulated annealing algorithm used to find the minimum
/// delta longitude/latitude. Basically, it just calls 
/// <see cref="calculateLonLatDeltaOfPoint" /> function.
/// </summary>
let srtmMinCellEnergy scaleFactor: EnergyFunc<MinLonLatDeltaState> =
    fun (rasterX, rasterY) ->
        calculateLonLatDeltaOfPoint rasterX rasterY scaleFactor

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
/// Currently it only supports Web Mercator projection, but this will be fixed
/// once we start supporting custom map projections.
/// </summary>
let minLonLatDelta (rasterRect: Raster.Rect) scaleFactor: LonLatDelta =
    let initialState = (
        rasterRect.MinX + rasterRect.Width / 2,
        rasterRect.MinY + rasterRect.Height / 2 )

    let finalMinDeltaPoint = 
        simulatedAnnealing 
            (annealingScheduleExponential 100. 0.85) 
            initialState 
            (srtmMinCellDeltaNeighbor rasterRect)
            (srtmMinCellEnergy scaleFactor)
            kirkpatrickAcceptanceProbability
            1000 
    
    srtmMinCellEnergy scaleFactor finalMinDeltaPoint

/// <summary>
/// Calculates the required SRTM level for a given lon/lat delta.
/// </summary>
let lonLatDeltaToSrtmLevel (lonLatDelta: LonLatDelta): SrtmLevel = 
    min MaxSrtmLevel (max 0 (Math.Log2 lonLatDelta |> int)) 
    |> SrtmLevel.fromInt
