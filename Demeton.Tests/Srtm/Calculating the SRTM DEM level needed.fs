module Tests.Srtm.``Calculating the SRTM DEM level needed``

open Demeton.Geometry.Common
open Demeton.Projections
open Demeton.Projections.Common
open Demeton.Srtm
open SimAnn
open System

open Xunit
open Swensen.Unquote

let area = 
    { MinLon = 4.702148; MinLat = 43.331571; 
    MaxLon = 16.976075; MaxLat = 48.580605 }

let private rasterRectFor scaleFactor =
    let minCornerX, minCornerY = 
        WebMercator.proj (area.MinLon |> degToRad) (area.MaxLat |> degToRad)
        |> Option.get
    let maxCornerX, maxCornerY = 
        WebMercator.proj (area.MaxLon |> degToRad) (area.MinLat |> degToRad) 
        |> Option.get

    Raster.Rect.asMinMax
        (int (floor (minCornerX * scaleFactor)))
        -(int (floor (minCornerY * scaleFactor)))
        (int (ceil (maxCornerX * scaleFactor)))
        -(int (ceil (maxCornerY * scaleFactor)))

let private rasterXYToLonLat (rasterX, rasterY) scaleFactor =
    WebMercator.inverse 
        ((float rasterX) / scaleFactor)
        -((float rasterY) / scaleFactor)

let private rasterToSrtmCoords (rasterX, rasterY) scaleFactor =
    let sampleLonLatMaybe = rasterXYToLonLat (rasterX, rasterY) scaleFactor

    match sampleLonLatMaybe with
    | None -> None
    | Some (lon, lat) -> 
        let tileX = Tile.longitudeToGlobalX lon 3600
        let tileY = Tile.latitudeToGlobalY lat 3600
        Some (tileX, tileY)

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
        (abs (lon1-lon0), abs (lat1-lat0))
    | _ -> (Double.MaxValue, Double.MaxValue)

let calculateLonLatDeltaOfSamplePoint
    scaleFactor
    (mapRasterBox: Raster.Rect)
    =
    let rasterSamplePointX = mapRasterBox.MinX + mapRasterBox.Width / 2
    let rasterSamplePointY = mapRasterBox.MinY + mapRasterBox.Height / 2

    calculateLonLatDeltaOfPoint 
        rasterSamplePointX rasterSamplePointY scaleFactor

[<Fact>]
let ``Can calculate distance between neighborhood rasters pixels in terms of SRTM DEM cells``() =
    let scaleFactor = { MapScale = 1000000.; Dpi = 1. }.ProjectionScaleFactor
    let (dx, dy) = 
        scaleFactor 
        |> rasterRectFor
        |> calculateLonLatDeltaOfSamplePoint scaleFactor

    test <@ abs (dx - 0.003986813104) < 0.0001 @>
    test <@ abs (dy - 0.00277597281) < 0.0001 @>

    let scaleFactor = { MapScale = 2000000.; Dpi = 1. }.ProjectionScaleFactor
    let (dx, dy) = 
        scaleFactor 
        |> rasterRectFor
        |> calculateLonLatDeltaOfSamplePoint scaleFactor

    test <@ abs (dx - 0.007973626208) < 0.0001 @>
    test <@ abs (dy - 0.005544002665) < 0.0001 @>

    let scaleFactor = { MapScale = 1000000.; Dpi = 2. }.ProjectionScaleFactor
    let (dx, dy) = 
        scaleFactor 
        |> rasterRectFor
        |> calculateLonLatDeltaOfSamplePoint scaleFactor

    test <@ abs (dx - 0.001993406552) < 0.0001 @>
    test <@ abs (dy - 0.001385007704) < 0.0001 @>

type private MinLonLatDeltaState = Raster.Point

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

let private srtmMinCellEnergy scaleFactor: EnergyFunc<MinLonLatDeltaState> =
    fun (rasterX, rasterY) ->
        let (dx, dy) = 
            calculateLonLatDeltaOfPoint rasterX rasterY scaleFactor
        min dx dy
    
/// <summary>
/// Calculates the minimum lon/lat delta using brute force by calculating it for
/// all raster pixels. This function is used for validating the result of the
/// simulated annealing.
/// </summary>
let private calculateMinDeltaUsingBruteForce 
    (rasterRect: Raster.Rect) scaleFactor =
    let points = 
        seq {
            for y in rasterRect.MinY .. rasterRect.MaxY do
                for x in rasterRect.MinX .. rasterRect.MaxX do
                    yield (x, y)
        }

    points 
    |> Seq.map (srtmMinCellEnergy scaleFactor)
    |> Seq.min

[<Fact>]
let ``Determines the lowest SRTM cell delta using simulated annealing``() =
    let scaleFactor = { MapScale = 1000000.; Dpi = 5. }.ProjectionScaleFactor
    let rasterRect = scaleFactor |> rasterRectFor

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

    let minDelta = calculateMinDeltaUsingBruteForce rasterRect scaleFactor

    printfn "minDelta: %g" minDelta

    test <@ abs ((srtmMinCellEnergy scaleFactor finalMinDeltaPoint) - minDelta) 
                < 0.001 @>
