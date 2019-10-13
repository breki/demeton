module ``Commands tests``.``ShadeCommand``.``Shading the raster``

open Demeton
open Demeton.Commands
open Demeton.DemTypes
open Png

open Xunit
open Swensen.Unquote

let coveragePoints = [(4.262676, 42.90816); (16.962471, 48.502048)]

let options: ShadeCommand.Options = {
        CoveragePoints = coveragePoints
        Dpi = 300.
        FileName = "shading"
        LocalCacheDir = "cache"
        MapScale = 5000000.
        OutputDir = "output"
        SrtmDir = "srtm"
        TileSize = 1000
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

    ShadeCommand.colorRasterBasedOnElevation 
        heightsArray tileRect imageData options

    let mutable anyNonColoredPixel = false
    for y in 0 .. (imageHeight-1) do
        for x in 0 .. (imageWidth-1) do
            let pixel = Rgba8Bit.pixelAt imageData imageWidth x y
            match Rgba8Bit.r pixel with
            | 0uy -> anyNonColoredPixel <- true
            | _ -> ()

    test <@ not anyNonColoredPixel @>

[<Fact(Skip="todo")>]
let ``Hillshader colors all of the image``() =
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

    ShadeCommand.shadeRaster 
        heightsArray tileRect imageData options

    let mutable anyNonColoredPixel = false
    for y in 0 .. (imageHeight-1) do
        for x in 0 .. (imageWidth-1) do
            let pixel = Rgba8Bit.pixelAt imageData imageWidth x y
            match Rgba8Bit.r pixel with
            | 0uy -> anyNonColoredPixel <- true
            | _ -> ()

    test <@ not anyNonColoredPixel @>


