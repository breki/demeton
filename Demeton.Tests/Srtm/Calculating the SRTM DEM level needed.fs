module Tests.Srtm.``Calculating the SRTM DEM level needed``

open Demeton.Geometry.Common
open Demeton.Projections.Common
open Demeton.Projections.Factory
open Demeton.Projections.MinLonLatDelta
open Demeton.Projections.PROJParsing

open Xunit
open Swensen.Unquote
open TestHelp

let area = 
    { MinLon = 4.702148; MinLat = 43.331571; 
    MaxLon = 16.976075; MaxLat = 48.580605 }

let private rasterRectFor projection =
    let minCornerX, minCornerY = 
        projection.Proj (area.MinLon |> degToRad) (area.MaxLat |> degToRad)
        |> Option.get
    let maxCornerX, maxCornerY = 
        projection.Proj (area.MaxLon |> degToRad) (area.MinLat |> degToRad) 
        |> Option.get

    Raster.Rect.asMinMax
        (int (floor minCornerX))
        -(int (floor minCornerY))
        (int (ceil maxCornerX))
        -(int (ceil maxCornerY))


let calculateLonLatDeltaOfSamplePoint
    mapProjection
    (mapRasterBox: Raster.Rect)
    =
    let rasterSamplePointX = mapRasterBox.MinX + mapRasterBox.Width / 2
    let rasterSamplePointY = mapRasterBox.MinY + mapRasterBox.Height / 2

    calculateLonLatDeltaOfPoint
        mapProjection.Invert 
        rasterSamplePointX rasterSamplePointY

[<Fact>]
let ``Can calculate distance between neighborhood rasters pixels in terms of SRTM DEM cells``() =
    let mapScale = { MapScale = 1000000.; Dpi = 1. }
    let projection =
        createMapProjection Mercator mapScale
        |> resultValue

    let delta = 
        projection 
        |> rasterRectFor
        |> calculateLonLatDeltaOfSamplePoint projection

    test <@ abs (delta - 0.00277597281) < 0.0001 @>

    let mapScale = { MapScale = 2000000.; Dpi = 1. }
    let projection =
        createMapProjection Mercator mapScale
        |> resultValue

    let delta = 
        projection 
        |> rasterRectFor
        |> calculateLonLatDeltaOfSamplePoint projection

    test <@ abs (delta - 0.005544002665) < 0.0001 @>

    let mapScale = { MapScale = 1000000.; Dpi = 2. }
    let projection =
        createMapProjection Mercator mapScale
        |> resultValue

    let delta = 
        projection 
        |> rasterRectFor
        |> calculateLonLatDeltaOfSamplePoint projection

    test <@ abs (delta - 0.001385007704) < 0.0001 @>

/// <summary>
/// Calculates the minimum lon/lat delta using brute force by calculating it for
/// all raster pixels. This function is used for validating the result of the
/// simulated annealing.
/// </summary>
let private calculateMinDeltaUsingBruteForce 
    (rasterRect: Raster.Rect) mapProjection =
    let points = 
        seq {
            for y in rasterRect.MinY .. rasterRect.MaxY do
                for x in rasterRect.MinX .. rasterRect.MaxX do
                    yield (x, y)
        }

    points 
    |> Seq.map (srtmMinCellEnergy mapProjection.Invert)
    |> Seq.min

[<Fact>]
let ``Determines the min lon/lat delta using simulated annealing``() =
    let mapScale = { MapScale = 1000000.; Dpi = 5. }
    let projection =
        createMapProjection Mercator mapScale
        |> resultValue
    let rasterRect = projection |> rasterRectFor

    let minDeltaUsingSimAnn =
        minLonLatDelta rasterRect projection.Invert
        
    let minDeltaUsingBruteForce = 
        calculateMinDeltaUsingBruteForce rasterRect projection

    test <@ abs (minDeltaUsingSimAnn - minDeltaUsingBruteForce) < 0.001 @>

[<Theory>]
[<InlineData(0.1, 0)>]
[<InlineData(1, 0)>]
[<InlineData(1.5, 0)>]
[<InlineData(2, 1)>]
[<InlineData(2.1, 1)>]
[<InlineData(3.5, 1)>]
[<InlineData(4.5, 2)>]
[<InlineData(2000, 6)>]
let ``Calculates the SRTM level needed from the lon/lat delta``
    (delta, expectedLevel) =

    let tileSize = 3600
    let actualRadDelta = 
        1. / (float tileSize) * delta |> Demeton.Geometry.Common.degToRad

    test <@ (lonLatDeltaToSrtmLevel tileSize actualRadDelta).Value = 
                expectedLevel @>
