[<RequireQualifiedAccess>]
module Tests.Shaders.ShadingSampleGenerator

open Demeton.Dem.Types
open Demeton.Geometry.Common
open Demeton.Projections.Common
open Demeton.Projections.Factory
open Demeton.Projections.PROJParsing
open Demeton.Projections.MinLonLatDelta
open Demeton.Srtm.Funcs
open Xunit
open Swensen.Unquote
open TestHelp

/// <summary>
/// Generates a sample geographic area bounds, its corresponding heights array
/// (filled with dummy height values) and calculates the raster rectangle needed
/// to cover the area. This function is used for shader tests.
/// </summary>
let generateSampleWithParameters minLon minLat maxLon maxLat mapScale dpi =
    let area =
        { MinLon = minLon
          MinLat = minLat
          MaxLon = maxLon
          MaxLat = maxLat }

    let mapScale = { MapScale = mapScale; Dpi = dpi }
    let projection = createMapProjection Mercator mapScale |> resultValue

    let minCornerX, minCornerY =
        projection.Proj (area.MinLon |> degToRad) (area.MaxLat |> degToRad)
        |> Option.get

    let maxCornerX, maxCornerY =
        projection.Proj (area.MaxLon |> degToRad) (area.MinLat |> degToRad)
        |> Option.get

    let rasterRect =
        Raster.Rect.asMinMax
            (int (floor minCornerX))
            -(int (floor minCornerY))
            (int (ceil maxCornerX))
            -(int (ceil maxCornerY))

    let srtmLevelNeeded =
        minLonLatDelta rasterRect projection.Invert
        |> lonLatDeltaToSrtmLevel 3600

    let minLonNeeded, minLatNeeded =
        projection.Invert
            (rasterRect.MinX - 1 |> float)
            -(rasterRect.MinY - 1 |> float)
        |> Option.get

    let maxLonNeeded, maxLatNeeded =
        projection.Invert
            (rasterRect.MaxX + 1 |> float)
            -(rasterRect.MaxY + 1 |> float)
        |> Option.get

    let minLonNeededDeg = radToDeg minLonNeeded
    let minLatNeededDeg = radToDeg minLatNeeded
    let maxLonNeededDeg = radToDeg maxLonNeeded
    let maxLatNeededDeg = radToDeg maxLatNeeded

    let cellsPerDegree = cellsPerDegree 3600 srtmLevelNeeded

    let minSrtmX =
        minLonNeededDeg |> longitudeToCellX cellsPerDegree |> floor |> int

    let minSrtmY =
        minLatNeededDeg |> latitudeToCellY cellsPerDegree |> floor |> int

    let maxSrtmX =
        maxLonNeededDeg |> longitudeToCellX cellsPerDegree |> floor |> int

    let maxSrtmY =
        maxLatNeededDeg |> latitudeToCellY cellsPerDegree |> floor |> int

    let heights =
        HeightsArray(
            minSrtmX,
            minSrtmY,
            maxSrtmX - minSrtmX + 1,
            maxSrtmY - minSrtmY + 1,
            HeightsArrayInitializer1D(fun _ -> DemHeight 1000)
        )

    (area, heights, srtmLevelNeeded, projection, mapScale, rasterRect)

/// <summary>
/// Generates a sample geographic area bounds, its corresponding heights array
/// (filled with dummy height values) and calculates the raster rectangle needed
/// to cover the area. This function is used for shader tests.
/// </summary>
let generateSample () =
    generateSampleWithParameters
        15.331473
        46.45726
        15.465991
        46.539525
        10000.
        1.

[<Fact>]
let ``Sample data is valid and sane`` () =
    let _, heights, srtmLevel, _, _, rasterRect = generateSample ()

    test <@ srtmLevel = DemLevel.fromInt 2 @>
    test <@ heights.Width = 129 @>
    test <@ heights.Height = 76 @>
    test <@ rasterRect.Width = 60 @>
    test <@ rasterRect.Height = 51 @>
