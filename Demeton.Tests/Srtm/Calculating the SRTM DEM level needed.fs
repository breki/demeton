module Tests.Srtm.``Calculating the SRTM DEM level needed``

open Demeton.Geometry.Common
open Demeton.Projections
open Demeton.Shaders.Types
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

let private rasterToSrtmCoords (rasterX, rasterY) scaleFactor =
    let sampleLonLatMaybe = 
        WebMercator.inverse 
            ((float rasterX) / scaleFactor)
            -((float rasterY) / scaleFactor)

    match sampleLonLatMaybe with
    | None -> None
    | Some (lon, lat) -> 
        let tileX = Tile.longitudeToGlobalX lon 3600
        let tileY = Tile.latitudeToGlobalY lat 3600
        Some (tileX, tileY)

let calculateSrtmDistanceOfSample
    rasterSamplePointX rasterSamplePointY
    scaleFactor
    =
    let (tx0, ty0) = 
        rasterToSrtmCoords 
            (rasterSamplePointX, rasterSamplePointY) scaleFactor
        |> Option.get
    let (tx1, ty1) = 
        rasterToSrtmCoords 
            (rasterSamplePointX + 1, rasterSamplePointY + 1) scaleFactor
        |> Option.get

    (abs (tx1-tx0), abs (ty1-ty0))

let calculateSrtmDistanceOfPixels
    scaleFactor
    (mapRasterBox: Raster.Rect)
    =
    let rasterSamplePointX = 
        mapRasterBox.MinX + mapRasterBox.Width / 2
    let rasterSamplePointY =
        mapRasterBox.MinY + mapRasterBox.Height / 2

    calculateSrtmDistanceOfSample 
        rasterSamplePointX rasterSamplePointY scaleFactor

[<Fact>]
let ``Can calculate distance between neighborhood rasters pixels in terms of SRTM DEM cells``() =
    let scaleFactor = { MapScale = 1000000.; Dpi = 1. }.ProjectionScaleFactor
    let (dx, dy) = 
        scaleFactor 
        |> rasterRectFor
        |> calculateSrtmDistanceOfPixels scaleFactor

    test <@ abs (dx - 14.35252717) < 0.0001 @>
    test <@ abs (dy - 9.993502117) < 0.0001 @>

    let scaleFactor = { MapScale = 2000000.; Dpi = 1. }.ProjectionScaleFactor
    let (dx, dy) = 
        scaleFactor 
        |> rasterRectFor
        |> calculateSrtmDistanceOfPixels scaleFactor

    test <@ abs (dx - 28.70505435) < 0.0001 @>
    test <@ abs (dy - 19.9584096) < 0.0001 @>

    let scaleFactor = { MapScale = 1000000.; Dpi = 2. }.ProjectionScaleFactor
    let (dx, dy) = 
        scaleFactor 
        |> rasterRectFor
        |> calculateSrtmDistanceOfPixels scaleFactor

    test <@ abs (dx - 7.176263587) < 0.0001 @>
    test <@ abs (dy - 4.986027734) < 0.0001 @>

type SrtmMinCellDeltaState = { RasterX: int; RasterY: int } 

let srtmMinCellDeltaNeighbor (rasterRect: Raster.Rect) : 
    NeighborFunc<SrtmMinCellDeltaState> = 
    fun (state: SrtmMinCellDeltaState) (rnd: Random) ->

    let rasterDx = rnd.Next(0, 10 + 1) - 5
    let rasterDy = rnd.Next(0, 10 + 1) - 5

    let newRasterX = state.RasterX + rasterDx
    let newRasterY = state.RasterY + rasterDy

    let newRasterX = min (max newRasterX rasterRect.MinX) rasterRect.MaxX
    let newRasterY = min (max newRasterY rasterRect.MinY) rasterRect.MaxY

    { RasterX = newRasterX; RasterY = newRasterY }

let srtmMinCellEnergy scaleFactor: EnergyFunc<SrtmMinCellDeltaState> =
    fun state ->
        let (dx, dy) = 
            calculateSrtmDistanceOfSample 
                state.RasterX state.RasterY scaleFactor
        min dx dy
    
/// <summary>
/// Calculates the minimum lon/lat delta using brute force by calculating it for
/// all raster pixels. This function is used for validating the result of the
/// simulated annealing.
/// </summary>
let calculateMinDeltaUsingBruteForce (rasterRect: Raster.Rect) scaleFactor =
    let points = 
        seq {
            for y in rasterRect.MinY .. rasterRect.MaxY do
                for x in rasterRect.MinX .. rasterRect.MaxX do
                    yield { RasterX = x; RasterY = y }
        }

    points 
    |> Seq.map (srtmMinCellEnergy scaleFactor)
    |> Seq.min

[<Fact>]
let ``Determines the lowest SRTM cell delta using simulated annealing``() =
    let scaleFactor = { MapScale = 1000000.; Dpi = 5. }.ProjectionScaleFactor
    let rasterRect = scaleFactor |> rasterRectFor

    let initialState = { 
        RasterX = rasterRect.MinX + rasterRect.Width / 2;
        RasterY = rasterRect.MinY + rasterRect.Height / 2
        }

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
