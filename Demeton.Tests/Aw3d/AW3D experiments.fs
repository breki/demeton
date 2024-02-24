module Tests.Aw3d.``AW3D experiments``


open System.IO
open Demeton.Commands
open Demeton.DemTypes
open Demeton.Projections.PROJParsing
open Demeton.Shaders
open FsUnit
open Tests.Shaders
open Xunit
open TestHelp
open BitMiracle.LibTiff.Classic
open Swensen.Unquote


let readAw3dHeightsFromStream (stream: Stream) : DemHeight[] =
    use tiffFile = Tiff.ClientOpen("InMemory", "r", stream, TiffStream())

    let width = unbox (tiffFile.GetField(TiffTag.IMAGEWIDTH).[0].Value)

    if width <> 3600 then
        failwithf $"Expected width 3600, but got %d{width}"

    let height = unbox (tiffFile.GetField(TiffTag.IMAGELENGTH).[0].Value)

    if height <> 3600 then
        failwithf $"Expected height 3600, but got %d{height}"

    let arraySize = width * height
    let heightsArray: DemHeight[] = Array.zeroCreate arraySize

    let samplesPerPixel: int16 =
        unbox (tiffFile.GetField(TiffTag.SAMPLESPERPIXEL).[0].Value)

    let bitsPerSample: int16 =
        unbox (tiffFile.GetField(TiffTag.BITSPERSAMPLE).[0].Value)

    let scanlineSize = tiffFile.ScanlineSize()
    let buffer = Array.zeroCreate<byte> scanlineSize

    let pixelBytesSize = int samplesPerPixel * (int bitsPerSample / 8)

    let mutable heightsWrittenCount = 0

    for row in 0 .. height - 1 do
        let success = tiffFile.ReadScanline(buffer, row)

        if not success then
            failwithf $"Failed to read scanline %d{row}"

        // Process the buffer to get the pixel data

        let mutable pixelStart = 0

        for col in 0 .. width - 1 do
            // read little-endian int16 value from pixelData
            let height = System.BitConverter.ToInt16(buffer, pixelStart)

            heightsArray.[heightsWrittenCount] <- height
            heightsWrittenCount <- heightsWrittenCount + 1

            pixelStart <- pixelStart + pixelBytesSize

    heightsArray


[<Fact>]
let ``Load AW3D into a DemHeight`` () =
    use tiffReadStream = sampleFileStream @"AW3D.ALPSMLC30_N046E006_DSM.tif"
    let demHeight = readAw3dHeightsFromStream tiffReadStream

    test <@ demHeight <> null @>

let area, heights, srtmLevel, mapProjection, mapScale, tileRect =
    ShadingSampleGenerator.generateSampleWithParameters
        6.1
        46.1
        6.9
        46.9
        250000.
        72.

let coveragePoints = [ (area.MinLon, area.MinLat); (area.MaxLon, area.MaxLat) ]

let options: ShadeCommand.Options =
    { CoveragePoints = coveragePoints
      FilePrefix = "shading"
      LocalCacheDir = "cache"
      OutputDir = "output"
      SrtmDir = "srtm"
      TileSize = 10000
      RootShadingStep =
        Pipeline.Common.IgorHillshading IgorHillshader.defaultParameters
      MapScale = mapScale
      MapProjection =
        { Projection = PROJParameters.Mercator
          IgnoredParameters = [] } }

[<Fact>]
let ``Generate hillshading from AW3D sample file`` () =
    use tiffReadStream = sampleFileStream @"AW3D.ALPSMLC30_N046E006_DSM.tif"
    let demHeight = readAw3dHeightsFromStream tiffReadStream

    let tileSize = 3600

    let tileId = Demeton.Srtm.Funcs.parseTileName "N46E006"
    let cellMinX, cellMinY = Demeton.Srtm.Funcs.tileMinCell tileSize tileId

    let fetchHeightsArray tileIds =
        // todo 0: calculate cell min x and y
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

    let pixelShader = IgorHillshader.shadePixel IgorHillshader.defaultParameters

    let createShaderFunction _ =
        Demeton.Shaders.Hillshading.shadeRaster pixelShader

    let generateTile =
        ShadeCommand.generateShadedRasterTile
            fetchHeightsArray
            createShaderFunction

    let saveTile =
        ShadeCommand.saveShadedRasterTile
            FileSys.ensureDirectoryExists
            FileSys.openFileToWrite
            Png.File.savePngToStream

    let result = ShadeCommand.run options generateTile saveTile
    test <@ result |> isOk @>
