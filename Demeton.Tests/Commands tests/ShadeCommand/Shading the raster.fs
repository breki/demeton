module ``Commands tests``.``ShadeCommand``.``Shading the raster``

open Demeton.Commands
open Demeton.DemTypes
open Demeton.Geometry.Common
open Demeton.Projections
open Demeton.Shaders
open Demeton.Srtm
open Png

open Xunit
open Swensen.Unquote

let coveragePoints = [(4.262676, 42.90816); (16.962471, 48.502048)]

let options: ShadeCommand.Options = {
        CoveragePoints = coveragePoints
        FilePrefix = "shading"
        LocalCacheDir = "cache"
        OutputDir = "output"
        SrtmDir = "srtm"
        TileSize = 1000
        RootShadingStep = Pipeline.Common.ElevationColoring
            { ColorScale = ElevationColoring.colorScaleMaperitive }
        ShaderOptions = { Dpi = 300.; MapScale = 5000000. }
    }

[<Fact>]
let ``Elevation colorer colors all of the image``() =
    let rasterTopLeftLonDeg = 16.5
    let rasterTopLeftLatDeg = 45.5
    let rasterTopLeftLonRad = degToRad rasterTopLeftLonDeg
    let rasterTopLeftLatRad = degToRad rasterTopLeftLatDeg
    let imageWidth = 10
    let imageHeight = 10

    let scaleFactor = options.ShaderOptions.ProjectionScaleFactor

    let rasterMinX, rasterMinY = 
        match WebMercator.proj rasterTopLeftLonRad rasterTopLeftLatRad with
        | Some (x, y) -> (x * scaleFactor, y * -scaleFactor)
        | None -> invalidOp "bug"
    let rasterMaxX = rasterMinX + float imageWidth
    let rasterMaxY = rasterMinY + float imageHeight
    let (rasterBottomRightLonRad, rasterBottomRightLatRad) =
        match WebMercator.inverse 
            (rasterMaxX / scaleFactor) (rasterMaxY / -scaleFactor) with
        | Some (x, y) -> (x, y)
        | None -> invalidOp "bug"
    let rasterBottomRightLonDeg = radToDeg rasterBottomRightLonRad
    let rasterBottomRightLatDeg = radToDeg rasterBottomRightLatRad

    let tileRect: Raster.Rect = 
        { MinX = int (floor rasterMinX); MinY = int (floor rasterMinY); 
            Width = imageWidth; Height = imageHeight }

    let imageData =
        Rgba8Bit.createImageData imageWidth imageHeight Rgba8Bit.ImageDataZero

    let safetyBuffer = 10
    let heightsArrayMinX = 
        (Tile.longitudeToGlobalX rasterTopLeftLonDeg 3600
        |> floor |> int) - safetyBuffer
    let heightsArrayMinY = 
        (Tile.latitudeToGlobalY rasterTopLeftLatDeg 3600 
        |> floor |> int) - safetyBuffer
    let heightsArrayMaxX = 
        (Tile.longitudeToGlobalX rasterBottomRightLonDeg 3600 
        |> ceil |> int) + safetyBuffer
    let heightsArrayMaxY = 
        (Tile.latitudeToGlobalY rasterBottomRightLatDeg 3600
        |> ceil |> int) + safetyBuffer
    let heightsArrayWidth = heightsArrayMaxX - heightsArrayMinX + 1
    let heightsArrayHeight = heightsArrayMaxY - heightsArrayMinY + 1

    let heightsArray = 
        HeightsArray(
            heightsArrayMinX, heightsArrayMinY, 
            heightsArrayWidth, heightsArrayHeight, 
            HeightsArrayInitializer1D (fun _ -> DemHeight 1000))

    ElevationColoring.shadeRaster 
        ElevationColoring.colorScaleMaperitive
        heightsArray 
        tileRect 
        imageData 
        options.ShaderOptions

    let mutable anyNonColoredPixel = false
    for y in 0 .. (imageHeight-1) do
        for x in 0 .. (imageWidth-1) do
            let pixel = Rgba8Bit.pixelAt imageData imageWidth x y
            match Rgba8Bit.r pixel with
            | 0uy -> anyNonColoredPixel <- true
            | _ -> ()

    test <@ not anyNonColoredPixel @>

