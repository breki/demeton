module ``Commands tests``.``ShadeCommand``.``Shading the raster``

open Demeton.Commands
open Demeton.Geometry.Common
open Demeton.Shaders
open Demeton.Srtm
open Png

open Xunit
open Swensen.Unquote
open Tests.Shaders


let (area, heights, mapScale, tileRect) = 
    ShadingSampleGenerator.generateSample()

let coveragePoints = [(area.MinLon, area.MinLat); (area.MaxLon, area.MaxLat)]

let options: ShadeCommand.Options = {
        CoveragePoints = coveragePoints
        FilePrefix = "shading"
        LocalCacheDir = "cache"
        OutputDir = "output"
        SrtmDir = "srtm"
        TileSize = 1000
        RootShadingStep = Pipeline.Common.ElevationColoring
            { ColorScale = ElevationColoring.colorScaleMaperitive }
        MapScale = mapScale
    }

[<Fact>]
let ``Elevation colorer colors all of the image``() =
    let imageWidth = tileRect.Width
    let imageHeight = tileRect.Height
    let imageData =
        Rgba8Bit.createImageData 
            imageWidth imageHeight Rgba8Bit.ImageDataZero

    ElevationColoring.shadeRaster 
        ElevationColoring.colorScaleMaperitive
        heights
        tileRect 
        imageData 
        options.MapScale

    let mutable anyNonColoredPixel = false
    for y in 0 .. (imageHeight-1) do
        for x in 0 .. (imageWidth-1) do
            let pixel = Rgba8Bit.pixelAt imageData imageWidth x y
            match Rgba8Bit.r pixel with
            | 0uy -> anyNonColoredPixel <- true
            | _ -> ()

    test <@ not anyNonColoredPixel @>

