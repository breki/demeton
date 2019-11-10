[<RequireQualifiedAccess>]
module Tests.Shaders.ShadingSampleGenerator

open Demeton.DemTypes
open Demeton.Geometry.Common
open Demeton.Projections
open Demeton.Projections.Common
open Demeton.Projections.MinLonLatDelta
open Demeton.Srtm
open Xunit
open Swensen.Unquote
open Demeton.Srtm.Types

/// <summary>
/// Generates a sample geographic area bounds, its corresponding heights array
/// (filled with dummy height values) and calculates the raster rectangle needed
/// to cover the area. This function is used for shader tests.
/// </summary>
let generateSampleWithParameters 
    minLon minLat maxLon maxLat mapScale dpi =
    let area = 
        { MinLon = minLon; MinLat = minLat; MaxLon = maxLon; MaxLat = maxLat }

    let mapScale = { MapScale = mapScale; Dpi = dpi }
    let scaleFactor = mapScale.ProjectionScaleFactor

    let minCornerX, minCornerY = 
        WebMercator.proj (area.MinLon |> degToRad) (area.MaxLat |> degToRad)
        |> Option.get
    let maxCornerX, maxCornerY = 
        WebMercator.proj (area.MaxLon |> degToRad) (area.MinLat |> degToRad) 
        |> Option.get

    let rasterRect = 
        Raster.Rect.asMinMax
            (int (floor (minCornerX * scaleFactor)))
            -(int (floor (minCornerY * scaleFactor)))
            (int (ceil (maxCornerX * scaleFactor)))
            -(int (ceil (maxCornerY * scaleFactor)))

    let srtmLevelNeeded =
        minLonLatDelta rasterRect scaleFactor
        |> lonLatDeltaToSrtmLevel 3600

    let (minLonNeeded, minLatNeeded) = 
        WebMercator.inverse 
            ((rasterRect.MinX - 1 |> float) / scaleFactor)
            -((rasterRect.MinY - 1 |> float) / scaleFactor)
        |> Option.get
    let (maxLonNeeded, maxLatNeeded) = 
        WebMercator.inverse 
            ((rasterRect.MaxX + 1 |> float) / scaleFactor)
            -((rasterRect.MaxY + 1 |> float) / scaleFactor)
        |> Option.get

    let minLonNeededDeg = radToDeg minLonNeeded 
    let minLatNeededDeg = radToDeg minLatNeeded 
    let maxLonNeededDeg = radToDeg maxLonNeeded 
    let maxLatNeededDeg = radToDeg maxLatNeeded 

    let minSrtmX = 
        Tile.longitudeToGlobalX minLonNeededDeg srtmLevelNeeded 3600 
        |> floor |> int
    let minSrtmY = 
        Tile.latitudeToGlobalY minLatNeededDeg srtmLevelNeeded 3600 
        |> floor |> int
    let maxSrtmX = 
        Tile.longitudeToGlobalX maxLonNeededDeg srtmLevelNeeded 3600 
        |> floor |> int
    let maxSrtmY = 
        Tile.latitudeToGlobalY maxLatNeededDeg srtmLevelNeeded 3600 
        |> floor |> int

    let heights = 
        HeightsArray
            (minSrtmX, minSrtmY, 
            maxSrtmX - minSrtmX + 1, maxSrtmY - minSrtmY + 1, 
            HeightsArrayInitializer1D(fun _ -> DemHeight 1000))

    (area, heights, srtmLevelNeeded, mapScale, rasterRect)

/// <summary>
/// Generates a sample geographic area bounds, its corresponding heights array
/// (filled with dummy height values) and calculates the raster rectangle needed
/// to cover the area. This function is used for shader tests.
/// </summary>
let generateSample() =
    generateSampleWithParameters 
        15.331473 46.45726 15.465991 46.539525 10000. 1.

[<Fact>]
let ``Sample data is valid and sane``() =
    let (_, heights, srtmLevel, _, rasterRect) = generateSample()

    test <@ srtmLevel = SrtmLevel.fromInt 2 @>
    test <@ heights.Width = 129 @>
    test <@ heights.Height = 76 @>
    test <@ rasterRect.Width = 60 @>
    test <@ rasterRect.Height = 51 @>
