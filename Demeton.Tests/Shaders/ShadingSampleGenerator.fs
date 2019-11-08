[<RequireQualifiedAccess>]
module Tests.Shaders.ShadingSampleGenerator

open Demeton.DemTypes
open Demeton.Geometry.Common
open Demeton.Projections
open Demeton.Shaders.Types
open Demeton.Srtm
open Xunit
open Swensen.Unquote

/// <summary>
/// Generates a sample geographic area bounds, its corresponding heights array
/// (filled with dummy height values) and calculates the raster rectangle needed
/// to cover the area. This function is used for shader tests.
/// </summary>
let generateSample() =
    let area = 
        { MinLon = 15.331473; MinLat = 46.45726; 
        MaxLon = 15.465991; MaxLat = 46.539525 }

    let shaderOptions = { MapScale = 100000.; Dpi = 1. }

    let minCornerX, minCornerY = 
        WebMercator.proj (area.MinLon |> degToRad) (area.MaxLat |> degToRad)
        |> Option.get
    let maxCornerX, maxCornerY = 
        WebMercator.proj (area.MaxLon |> degToRad) (area.MinLat |> degToRad) 
        |> Option.get

    let rasterRect = 
        Raster.Rect.asMinMax
            (int (floor (minCornerX * shaderOptions.ProjectionScaleFactor)))
            -(int (floor (minCornerY * shaderOptions.ProjectionScaleFactor)))
            (int (ceil (maxCornerX * shaderOptions.ProjectionScaleFactor)))
            -(int (ceil (maxCornerY * shaderOptions.ProjectionScaleFactor)))

    let (minLonNeeded, minLatNeeded) = 
        WebMercator.inverse 
            ((rasterRect.MinX - 1 |> float) / shaderOptions.ProjectionScaleFactor)
            -((rasterRect.MinY - 1 |> float) / shaderOptions.ProjectionScaleFactor)
        |> Option.get
    let (maxLonNeeded, maxLatNeeded) = 
        WebMercator.inverse 
            ((rasterRect.MaxX + 1 |> float) / shaderOptions.ProjectionScaleFactor)
            -((rasterRect.MaxY + 1 |> float) / shaderOptions.ProjectionScaleFactor)
        |> Option.get

    let minLonNeededDeg = radToDeg minLonNeeded 
    let minLatNeededDeg = radToDeg minLatNeeded 
    let maxLonNeededDeg = radToDeg maxLonNeeded 
    let maxLatNeededDeg = radToDeg maxLatNeeded 

    let minSrtmX = Tile.longitudeToGlobalX minLonNeededDeg 3600 |> floor |> int
    let minSrtmY = Tile.latitudeToGlobalY minLatNeededDeg 3600 |> floor |> int
    let maxSrtmX = Tile.longitudeToGlobalX maxLonNeededDeg 3600 |> floor |> int
    let maxSrtmY = Tile.latitudeToGlobalY maxLatNeededDeg 3600 |> floor |> int

    let heights = 
        HeightsArray
            (minSrtmX, minSrtmY, 
            maxSrtmX - minSrtmX + 1, maxSrtmY - minSrtmY + 1, 
            HeightsArrayInitializer1D(fun _ -> DemHeight 1000))

    (area, heights, shaderOptions, rasterRect)

[<Fact>]
let ``Sample data is valid and sane``() =
    let (_, heights, _, rasterRect) = generateSample()

    test <@ heights.Width = 741 @>
    test <@ heights.Height = 340 @>
    test <@ rasterRect.Width = 7 @>
    test <@ rasterRect.Height = 4 @>
