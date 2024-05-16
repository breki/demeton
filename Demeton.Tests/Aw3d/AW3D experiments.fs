module Tests.Aw3d.``AW3D experiments``


open System
open Demeton.Aw3d.Types
open Demeton.Commands
open Demeton.DemTypes
open Demeton.Projections.PROJParsing
open Demeton.Shaders
open FsUnit
open Png
open Tests.Shaders
open Xunit
open TestHelp
open BitMiracle.LibTiff.Classic
open Swensen.Unquote


// todo 0: transform into an AW3D tile loading function
let readAw3dHeights
    cacheDir
    (tileId: Aw3dTileId)
    (fileName: string)
    : DemHeight[] =
    use tiff = Tiff.Open(fileName, "r")

    let width = unbox (tiff.GetField(TiffTag.IMAGEWIDTH).[0].Value)

    if width <> 3600 then
        failwithf $"Expected width 3600, but got %d{width}"

    let height = unbox (tiff.GetField(TiffTag.IMAGELENGTH).[0].Value)

    if height <> 3600 then
        failwithf $"Expected height 3600, but got %d{height}"

    let arraySize = width * height
    let heightsArray: DemHeight[] = Array.zeroCreate arraySize

    let samplesPerPixel: int16 =
        unbox (tiff.GetField(TiffTag.SAMPLESPERPIXEL).[0].Value)

    let bitsPerSample: int16 =
        unbox (tiff.GetField(TiffTag.BITSPERSAMPLE).[0].Value)

    let scanlineSize = tiff.ScanlineSize()
    let buffer = Array.zeroCreate<byte> scanlineSize

    let pixelBytesSize = int samplesPerPixel * (int bitsPerSample / 8)

    let mutable heightsWrittenCount = 0

    for row in 0 .. height - 1 do
        let success = tiff.ReadScanline(buffer, row)

        if not success then
            failwithf $"Failed to read scanline %d{row}"

        // Process the buffer to get the pixel data

        let mutable pixelStart = 0

        for col in 0 .. width - 1 do
            // read little-endian int16 value from pixelData
            let height = BitConverter.ToInt16(buffer, pixelStart)

            heightsArray.[heightsWrittenCount] <- height
            heightsWrittenCount <- heightsWrittenCount + 1

            pixelStart <- pixelStart + pixelBytesSize

    heightsArray


[<Fact>]
let ``Load AW3D into a DemHeight`` () =
    let fileName = @"samples\ALPSMLC30_N046E007_DSM.tif"
    let tileId = { TileX = 46; TileY = -7 }
    let demHeight = readAw3dHeights "cache" tileId fileName

    test <@ demHeight <> null @>

let area, heights, srtmLevel, mapProjection, mapScale, tileRect =
    // ShadingSampleGenerator.generateSampleWithParameters
    //     7.1
    //     46.1
    //     7.9
    //     46.9
    //     250000.
    //     72.
    // ShadingSampleGenerator.generateSampleWithParameters
    //     7.416765
    //     46.613756
    //     7.928785
    //     46.772998
    //     25000.
    //     72.
    ShadingSampleGenerator.generateSampleWithParameters
        7.416765
        46.613756
        7.628785
        46.652998
        25000.
        72.

let coveragePoints = [ (area.MinLon, area.MinLat); (area.MaxLon, area.MaxLat) ]

let options: ShadeCommand.Options =
    { CoveragePoints = coveragePoints
      FilePrefix = "shading"
      LocalCacheDir = "cache"
      OutputDir = "output"
      SrtmDir = "srtm"
      TileSize = 10000
      RootShadingStep = Pipeline.Common.CustomShading "XCTracer"
      MapScale = mapScale
      MapProjection =
        { Projection = PROJParameters.Mercator
          IgnoredParameters = [] } }

let xcTracerHillshader
    (parameters: IgorHillshader.ShaderParameters)
    : Hillshading.PixelHillshader =
    fun _ slope aspect ->
        match Double.IsNaN(aspect) with
        | true -> Rgba8Bit.TransparentColor
        | false ->
            let aspectDiff =
                Demeton.Geometry.Common.differenceBetweenAngles
                    aspect
                    parameters.SunAzimuth
                    (Math.PI * 2.)

            let slopeDarkness = slope / (Math.PI / 2.)
            let aspectDarkness = aspectDiff / Math.PI
            let darkness = slopeDarkness * aspectDarkness

            // let darknessByteLimit = 220uy
            //
            // let darknessByte =
            //     darknessByteLimit
            //     - Hillshading.colorComponentRatioToByteLimited
            //         darknessByteLimit
            //         darkness

            // let darknessByte = Hillshading.colorComponentRatioToByte darkness

            if aspectDarkness > 1. || aspectDarkness < 0. then
                Rgba8Bit.rgbColor 255uy 0uy 0uy
            else
                let darknessByte =
                    255uy - Hillshading.colorComponentRatioToByte darkness

                Rgba8Bit.rgbColor darknessByte darknessByte darknessByte

[<Literal>]
let tileSize = 3600

let fetchAw3dHeightsArray _ =
    let fileName = @"Samples\ALPSMLC30_N046E007_DSM.tif"

    let tileId = { TileX = 46; TileY = -7 }
    let demHeight = readAw3dHeights "cache" tileId fileName

    let tileId = Demeton.Srtm.Funcs.parseTileName "N46E007"
    let cellMinX, cellMinY = Demeton.Srtm.Funcs.tileMinCell tileSize tileId

    let cellMinX = cellMinX
    let cellMinY = cellMinY

    HeightsArray(
        cellMinX,
        cellMinY,
        tileSize,
        tileSize,
        HeightsArrayDirectImport demHeight
    )
    |> Some
    |> Result.Ok


[<Fact>]
let ``Generate hillshading from AW3D sample file`` () =
    let pixelShader = xcTracerHillshader IgorHillshader.defaultParameters

    let createShaderFunction _ =
        Demeton.Shaders.Hillshading.shadeRaster 0 pixelShader

    // todo 5: can we reuse these functions for the new command?
    let generateTile =
        ShadeCommand.generateShadedRasterTile
            [| fetchAw3dHeightsArray |]
            createShaderFunction

    let saveTile =
        ShadeCommand.saveShadedRasterTile
            FileSys.ensureDirectoryExists
            FileSys.openFileToWrite
            File.savePngToStream

    let result = ShadeCommand.run options generateTile saveTile
    test <@ result |> isOk @>
