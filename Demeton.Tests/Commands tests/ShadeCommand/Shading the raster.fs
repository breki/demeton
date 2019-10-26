module ``Commands tests``.``ShadeCommand``.``Shading the raster``

open Demeton
open Demeton.Commands
open Demeton.DemTypes
open Demeton.Shaders
open Demeton.Shaders.ShaderTypes
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
        RootShadingStep = 
            ShadingPipeline.Shading (ElevationColoring.shadeRaster)
        ShaderOptions = { Dpi = 300.; MapScale = 5000000. }
    }

[<Fact>]
let ``Elevation colorer colors all of the image``() =
    let imageWidth = 10
    let imageHeight = 10

    let tileRect: Raster.Rect = 
        { MinX = 1119; MinY = 12500; Width = imageWidth; Height = imageHeight }

    let imageData =
        Rgba8Bit.createImageData imageWidth imageHeight Rgba8Bit.ImageDataZero

    let heightsArray = 
        HeightsArray(
            659736, 478459, 1000, 1000, HeightsArrayInitializer1D (
                fun _ -> DemHeight 1000))

    ElevationColoring.shadeRaster 
        heightsArray tileRect imageData options.ShaderOptions

    let mutable anyNonColoredPixel = false
    for y in 0 .. (imageHeight-1) do
        for x in 0 .. (imageWidth-1) do
            let pixel = Rgba8Bit.pixelAt imageData imageWidth x y
            match Rgba8Bit.r pixel with
            | 0uy -> anyNonColoredPixel <- true
            | _ -> ()

    test <@ not anyNonColoredPixel @>

