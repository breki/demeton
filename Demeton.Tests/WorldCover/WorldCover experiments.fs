module Tests.WorldCover.``WorldCover experiments``


open System
open System.Threading.Tasks
open Demeton.Commands
open Demeton.DemTypes
open Demeton.Geometry.Common
open Demeton.Projections.PROJParsing
open Demeton.Shaders
open Demeton.Shaders.Types
open Demeton.Srtm.Funcs
open Demeton.Srtm.Types
open FsUnit
open Png
open Tests.Shaders
open Xunit
open BitMiracle.LibTiff.Classic
open Swensen.Unquote
open TestHelp

/// <summary>
/// Reads a WorldCover raster file and returns the heights array for a specified
/// tile.
/// </summary>
/// <remarks>
/// The function accepts the SRTM-like tile coordinates of the lower left corner
/// of the WorldCover raster file and the SRTM-like tile coordinates of the tile
/// whose heights array is to be read.
/// </remarks>
let readWorldCoverRaster
    (fileName: string)
    (raster_file_tile_coords: SrtmTileCoords)
    (requested_tile_coords: SrtmTileCoords)
    : DemHeight[] =
    use tiff = Tiff.Open(fileName, "r")

    if tiff = null then
        failwithf $"Could not open the file %s{fileName}"

    let rasterWidth = unbox (tiff.GetField(TiffTag.IMAGEWIDTH).[0].Value)

    if rasterWidth <> 36000 then
        failwithf $"Expected width 36000, but got %d{rasterWidth}"

    let rasterHeight = unbox (tiff.GetField(TiffTag.IMAGELENGTH).[0].Value)

    if rasterHeight <> 36000 then
        failwithf $"Expected height 36000, but got %d{rasterHeight}"

    let planarConfig: PlanarConfig =
        tiff.GetField(TiffTag.PLANARCONFIG).[0].Value :?> PlanarConfig

    if planarConfig <> PlanarConfig.CONTIG then
        failwithf
            $"Expected CONTIG planar configuration, but got %A{planarConfig}"

    if not (tiff.IsTiled()) then
        failwithf "Expected a tiled TIFF file"

    let pixelsPerSrtmTile = 36000 / 3

    let unreducedHeightsArrayWidth = pixelsPerSrtmTile
    let unreducedHeightsArrayHeight = pixelsPerSrtmTile

    let unreducedHeightsArray: DemHeight[] =
        Array.zeroCreate (
            unreducedHeightsArrayWidth * unreducedHeightsArrayHeight
        )

    let srtmHeightArray = Array.zeroCreate (3600 * 3600)

    let startingTiffTileX =
        (requested_tile_coords.Lon.Value - raster_file_tile_coords.Lon.Value)
        * pixelsPerSrtmTile

    let startingTiffTileY =
        -(requested_tile_coords.Lat.Value - raster_file_tile_coords.Lat.Value)
        * pixelsPerSrtmTile

    let endingTiffTileX = startingTiffTileX + pixelsPerSrtmTile
    let endingTiffTileY = startingTiffTileY + pixelsPerSrtmTile

    let tiffTileWidth = unbox (tiff.GetField(TiffTag.TILEWIDTH).[0].Value)
    let tiffTileLength = unbox (tiff.GetField(TiffTag.TILELENGTH).[0].Value)

    // the size (in bytes) of a single TIFF tile
    let tiffTileBufferSize = tiff.TileSize()
    let tiffTileBuffer = Array.zeroCreate<byte> tiffTileBufferSize

    let reductionFactor = float pixelsPerSrtmTile / (float 3600)

    for tiffTileY in [ startingTiffTileY..tiffTileLength..endingTiffTileY ] do
        for tiffTileX in [ startingTiffTileX..tiffTileWidth..endingTiffTileX ] do
            /// 0-based byte offset in buffer at which to begin storing read
            /// and decoded bytes
            let offset = 0
            /// z-coordinate of the pixel within a tile to be read and decoded
            let tileZ = 0
            /// The zero-based index of the sample plane. The plane parameter is
            /// used only if data are organized in separate planes
            /// (PLANARCONFIG = SEPARATE). In other cases the value is ignored.
            let plane = int16 0

            // The tile to read and decode is selected by the (x, y, z, plane)
            // coordinates (i.e. ReadTile returns the data for the tile
            // containing the specified coordinates.
            let bytesInTile =
                tiff.ReadTile(
                    tiffTileBuffer,
                    offset,
                    tiffTileX,
                    tiffTileY,
                    tileZ,
                    plane
                )

            if bytesInTile = -1 then
                failwith "Could not read tile."

            // Copy the contents of the tile buffer into the unreduced heights
            // array. This way we fill the heights array by the contents of each
            // TIFF tile, one by one, until we have the whole heights array
            // filed.
            // Note that we use int16 heights array here because of simplicity,
            // but in future we should provide direct support for byte arrays
            // (since WorldCover uses byte arrays).

            // todo 50: not exactly optimal way to copy from one array to another
            for i in 0 .. tiffTileBufferSize - 1 do
                let value = tiffTileBuffer.[i]

                // local coordinates of the pixel within the TIFF tile
                let tiffTileLocalX = i % tiffTileWidth
                let tiffTileLocalY = i / tiffTileWidth

                // coordinates of the pixel within the heights array
                let heightsArrayX =
                    (tiffTileX - startingTiffTileX) + tiffTileLocalX

                let heightsArrayY =
                    (tiffTileY - startingTiffTileY) + tiffTileLocalY

                // copy the pixel to the heights array only if it fits within
                // the array (some TIFF tiles may be partially outside the
                // requested area)
                if
                    heightsArrayX < unreducedHeightsArrayWidth
                    && heightsArrayY < unreducedHeightsArrayHeight
                then
                    // index of the pixel within the heights array
                    let index =
                        heightsArrayY * unreducedHeightsArrayWidth
                        + heightsArrayX

                    unreducedHeightsArray.[index] <- int16 value

            // now reduce the heightsArray to the SRTM size

            // todo 20: not really a good downsampling since it just takes the
            //   a single pixel value from a larger block of pixels

            // todo 15: maybe we don't need to downsample it, just is it as it
            //   is directly when shading?
            for y in 0..3599 do
                for x in 0..3599 do
                    let srtmIndex = y * 3600 + x

                    let unreducedIndex =
                        y * pixelsPerSrtmTile / 3600 * pixelsPerSrtmTile
                        + x * pixelsPerSrtmTile / 3600

                    srtmHeightArray.[srtmIndex] <-
                        unreducedHeightsArray.[unreducedIndex]

    srtmHeightArray


let area, heights, srtmLevel, mapProjection, mapScale, tileRect =
    ShadingSampleGenerator.generateSampleWithParameters
        7.1
        46.1
        7.9
        46.9
        250000.
        72.

let coveragePoints = [ (area.MinLon, area.MinLat); (area.MaxLon, area.MaxLat) ]

[<Literal>]
let StepNameXcTracerHillshading = "XCTracer-hillshading"

[<Literal>]
let StepNameXcTracerWaterBodies = "XCTracer-water-bodies"

let hillshadingStep = Pipeline.Common.CustomShading StepNameXcTracerHillshading
let waterBodiesStep = Pipeline.Common.CustomShading StepNameXcTracerWaterBodies

let hillAndWaterStep = hillshadingStep
// Pipeline.Common.Compositing(
//     hillshadingStep,
//     waterBodiesStep,
//     Demeton.Shaders.Pipeline.Common.CompositingFuncIdOver
// )


let options: ShadeCommand.Options =
    { CoveragePoints = coveragePoints
      FilePrefix = "shading"
      LocalCacheDir = "cache"
      OutputDir = "output"
      SrtmDir = "srtm"
      TileSize = 10000
      RootShadingStep = hillAndWaterStep
      MapScale = mapScale
      MapProjection =
        { Projection = PROJParameters.Mercator
          IgnoredParameters = [] } }

/// <summary>
/// Returns a raster shader that accepts a WorldCover-originated heights array
/// and renders water bodies in blue and everything else in transparent.
/// </summary>
let worldCoverWaterBodiesShader: RasterShader =
    fun heightsArrays srtmLevel tileRect imageData inverse ->
        let cellsPerDegree = cellsPerDegree 3600 srtmLevel

        let tileWidth = tileRect.Width

        let valueForTilePixel x y : DemHeight option =
            let lonLatOption = inverse (float x) (float -y)

            match lonLatOption with
            | None -> None
            | Some(lonRad, latRad) ->
                let lonDeg = radToDeg lonRad
                let latDeg = radToDeg latRad

                let globalSrtmX =
                    lonDeg
                    |> longitudeToCellX cellsPerDegree
                    |> Math.Round
                    |> int

                let globalSrtmY =
                    latDeg
                    |> latitudeToCellY cellsPerDegree
                    |> Math.Round
                    |> int

                heightsArrays[0].heightAt (globalSrtmX, globalSrtmY) |> Some

        let processRasterLine y =
            for x in tileRect.MinX .. (tileRect.MaxX - 1) do
                let rasterValue = valueForTilePixel x y

                let pixelValue =
                    match rasterValue with
                    // 80 represents water
                    | Some 80s -> Rgba8Bit.rgbColor 0uy 0uy 255uy
                    | _ -> Rgba8Bit.rgbaColor 0uy 0uy 0uy 0uy

                Rgba8Bit.setPixelAt
                    imageData
                    tileWidth
                    (x - tileRect.MinX)
                    (y - tileRect.MinY)
                    pixelValue

        Parallel.For(tileRect.MinY, tileRect.MaxY, processRasterLine) |> ignore

// todo 5: how do I combine shading of AW3D and WorldCover water bodies?

// skip this test if running on GitHub Actions
[<Fact>]
let ``Load WorldCover file into a DemHeight`` () =
    if Environment.GetEnvironmentVariable("CI") = "true" then
        // this test cannot run on CI because we don't have the WorldCover
        // raster available (it's too big to be added to git repo)
        ()
    else
        let demHeight =
            readWorldCoverRaster
                @"C:\temp\WorldCover\ESA_WorldCover_10m_2021_v200_N45E006_Map.tif"
                { Lon = { Value = 6 }
                  Lat = { Value = -45 } }
                { Lon = { Value = 7 }
                  Lat = { Value = -46 } }

        let tileSize = 3600

        let tileId = parseTileName "N46E007"
        let cellMinX, cellMinY = tileMinCell tileSize tileId

        let fetchWorldCoverHeightsArray tileIds =
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

        let createShaderFunction shaderFunctionName =
            match shaderFunctionName with
            | StepNameXcTracerWaterBodies -> worldCoverWaterBodiesShader
            | StepNameXcTracerHillshading ->
                Tests.Aw3d.``AW3D experiments``.xcTracerHillshader
                    IgorHillshader.defaultParameters
                |> Demeton.Shaders.Hillshading.shadeRaster 0
            | _ ->
                failwithf
                    $"Unknown shader function name: %s{shaderFunctionName}"

        let generateTile =
            ShadeCommand.generateShadedRasterTile
                [| fetchWorldCoverHeightsArray |]
                createShaderFunction

        let saveTile =
            ShadeCommand.saveShadedRasterTile
                FileSys.ensureDirectoryExists
                FileSys.openFileToWrite
                File.savePngToStream

        let result = ShadeCommand.run options generateTile saveTile
        test <@ result |> isOk @>
