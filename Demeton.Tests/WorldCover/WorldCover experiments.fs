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
open Tests.WorldCover.WaterBodiesColoring
open Xunit
open BitMiracle.LibTiff.Classic
open Swensen.Unquote
open TestHelp
open Tests.WorldCover.RasterSimplification

[<Literal>]
let WorldCoverTileSize = 12000

[<Literal>]
let WorldCoverBitmapSize = WorldCoverTileSize * 3


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

    if rasterWidth <> WorldCoverBitmapSize then
        failwithf
            $"Expected width %d{WorldCoverBitmapSize}, but got %d{rasterWidth}"

    let rasterHeight = unbox (tiff.GetField(TiffTag.IMAGELENGTH).[0].Value)

    if rasterHeight <> WorldCoverBitmapSize then
        failwithf
            $"Expected height %d{WorldCoverBitmapSize}, but got %d{rasterHeight}"

    let planarConfig: PlanarConfig =
        tiff.GetField(TiffTag.PLANARCONFIG).[0].Value :?> PlanarConfig

    if planarConfig <> PlanarConfig.CONTIG then
        failwithf
            $"Expected CONTIG planar configuration, but got %A{planarConfig}"

    if not (tiff.IsTiled()) then
        failwithf "Expected a tiled TIFF file"

    let unreducedHeightsArrayWidth = WorldCoverTileSize
    let unreducedHeightsArrayHeight = WorldCoverTileSize

    let worldCoverData: DemHeight[] =
        Array.zeroCreate (
            unreducedHeightsArrayWidth * unreducedHeightsArrayHeight
        )

    // calculate the box (in TIFF coordinates) of the requested SRTM tile
    let startingTiffX =
        (requested_tile_coords.Lon.Value - raster_file_tile_coords.Lon.Value)
        * WorldCoverTileSize

    let startingTiffY =
        -(requested_tile_coords.Lat.Value - raster_file_tile_coords.Lat.Value)
        * WorldCoverTileSize

    let endingTiffX = startingTiffX + WorldCoverTileSize
    let endingTiffY = startingTiffY + WorldCoverTileSize

    // the size of an individual TIFF tile (not geographic tile, but a tile
    // in terms of tiling a TIFF raster into small quadratic pieces)
    let tiffTileWidth = unbox (tiff.GetField(TiffTag.TILEWIDTH).[0].Value)
    let tiffTileHeight = unbox (tiff.GetField(TiffTag.TILELENGTH).[0].Value)

    // the memory size (in bytes) of a single TIFF tile
    let tiffTileBufferSize = tiff.TileSize()
    let tiffTileBuffer = Array.zeroCreate<byte> tiffTileBufferSize

    // for each TIFF tile that intersects the requested SRTM tile
    for tiffTileY in [ startingTiffY..tiffTileHeight..endingTiffY ] do
        for tiffTileX in [ startingTiffX..tiffTileWidth..endingTiffX ] do
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

            // The tile does not start at tiffTileX, tiffTileY, so we need to
            // calculate its actual starting coordinates and adjust
            // that in our calculations when copying to the unreducedHeightsArray

            let tileXIndex = tiffTileX / tiffTileWidth
            let tileYIndex = tiffTileY / tiffTileHeight

            let actualTileX = tileXIndex * tiffTileWidth
            let actualTileY = tileYIndex * tiffTileHeight

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
                    (actualTileX - startingTiffX) + tiffTileLocalX

                let heightsArrayY =
                    (actualTileY - startingTiffY) + tiffTileLocalY

                // copy the pixel to the heights array only if it fits within
                // the array (some TIFF tiles may be partially outside the
                // requested area)
                if
                    heightsArrayX >= 0
                    && heightsArrayY >= 0
                    && heightsArrayX < unreducedHeightsArrayWidth
                    && heightsArrayY < unreducedHeightsArrayHeight
                then
                    // index of the pixel within the heights array
                    let index =
                        heightsArrayY * unreducedHeightsArrayWidth
                        + heightsArrayX

                    worldCoverData.[index] <- int16 value

    worldCoverData


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

let hillAndWaterStep =
    Pipeline.Common.Compositing(
        hillshadingStep,
        waterBodiesStep,
        Demeton.Shaders.Pipeline.Common.CompositingFuncIdOver
    )


let options: ShadeCommand.Options =
    { CoveragePoints = coveragePoints
      FilePrefix = "XCTracer-hillshading-water"
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
let worldCoverWaterBodiesShader
    heightsArrayIndex
    (waterBodies: WaterBody list)
    : RasterShader =
    fun heightsArrays srtmLevel tileRect imageData inverse ->
        let cellsPerDegree = cellsPerDegree WorldCoverTileSize srtmLevel

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

                heightsArrays[heightsArrayIndex]
                    .heightAt (globalSrtmX, globalSrtmY)
                |> Some

        let waterColor =
            match (Rgba8Bit.tryParseColorHexValue "#49C8FF") with
            | Ok color -> color
            | Error _ -> failwith "Could not parse color"

        let ignoredWaterColor =
            match (Rgba8Bit.tryParseColorHexValue "#FF96D1") with
            | Ok color -> color
            | Error _ -> failwith "Could not parse color"

        let errorWaterColor =
            match (Rgba8Bit.tryParseColorHexValue "#FF4800") with
            | Ok color -> color
            | Error _ -> failwith "Could not parse color"

        let greenWaterColor = Rgba8Bit.rgbaColor 0uy 255uy 0uy 255uy
        let redWaterColor = Rgba8Bit.rgbaColor 255uy 0uy 0uy 255uy
        let blueWaterColor = Rgba8Bit.rgbaColor 0uy 0uy 255uy 255uy
        let yellowWaterColor = Rgba8Bit.rgbaColor 255uy 255uy 0uy 255uy
        let magentaWaterColor = Rgba8Bit.rgbaColor 255uy 0uy 255uy 255uy
        let cyanWaterColor = Rgba8Bit.rgbaColor 0uy 255uy 255uy 255uy

        let palette =
            [| greenWaterColor
               redWaterColor
               blueWaterColor
               yellowWaterColor
               magentaWaterColor
               cyanWaterColor |]

        let noWaterColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 0uy

        let processRasterLine y =
            for x in tileRect.MinX .. (tileRect.MaxX - 1) do
                let rasterValue = valueForTilePixel x y

                let pixelValue =
                    match rasterValue with
                    | None -> noWaterColor
                    | Some 0s -> noWaterColor
                    | Some 1s -> errorWaterColor
                    | Some waterBodyColor ->
                        let waterBody = waterBodies.[int waterBodyColor - 2]

                        let totalArea =
                            waterBody.Coverage.Width * waterBody.Coverage.Height

                        match
                            waterBody.SurfaceArea,
                            totalArea / waterBody.SurfaceArea
                        with
                        | surfaceArea, _ when surfaceArea < 1250 ->
                            ignoredWaterColor
                        | _, coverageRatio when coverageRatio >= 10 ->
                            ignoredWaterColor
                        | _ -> waterColor

                Rgba8Bit.setPixelAt
                    imageData
                    tileWidth
                    (x - tileRect.MinX)
                    (y - tileRect.MinY)
                    pixelValue

        Parallel.For(tileRect.MinY, tileRect.MaxY, processRasterLine) |> ignore

[<Fact>]
let ``Render hillshading with WorldCover water bodies`` () =
    if Environment.GetEnvironmentVariable("CI") = "true" then
        // this test cannot run on CI because we don't have the WorldCover
        // raster available (it's too big to be added to git repo)
        ()
    else
        let worldCoverData =
            readWorldCoverRaster
                @"C:\temp\WorldCover\ESA_WorldCover_10m_2021_v200_N45E006_Map.tif"
                { Lon = { Value = 6 }
                  Lat = { Value = -45 } }
                { Lon = { Value = 7 }
                  Lat = { Value = -46 } }

        let tileId = parseTileName "N46E007"
        let cellMinX, cellMinY = tileMinCell WorldCoverTileSize tileId

        let waterBodiesHeightsArray =
            HeightsArray(
                cellMinX,
                cellMinY,
                WorldCoverTileSize,
                WorldCoverTileSize,
                HeightsArrayDirectImport worldCoverData
            )
            |> convertWorldCoverRasterToWaterMonochrome
        // |> simplifyRaster 100

        let waterBodies = waterBodiesHeightsArray |> colorWaterBodies

        let fetchWorldCoverHeightsArray tileIds =
            waterBodiesHeightsArray |> Some |> Result.Ok

        let heightsArraysFetchers =
            [| Tests.Aw3d.``AW3D experiments``.fetchAw3dHeightsArray
               fetchWorldCoverHeightsArray |]

        let createShaderFunction shaderFunctionName =
            match shaderFunctionName with
            | StepNameXcTracerHillshading ->
                Tests.Aw3d.``AW3D experiments``.xcTracerHillshader
                    IgorHillshader.defaultParameters
                |> Demeton.Shaders.Hillshading.shadeRaster 0
            | StepNameXcTracerWaterBodies ->
                worldCoverWaterBodiesShader 1 waterBodies
            | _ ->
                failwithf
                    $"Unknown shader function name: %s{shaderFunctionName}"

        let generateTile =
            ShadeCommand.generateShadedRasterTile
                heightsArraysFetchers
                createShaderFunction

        let saveTile =
            ShadeCommand.saveShadedRasterTile
                FileSys.ensureDirectoryExists
                FileSys.openFileToWrite
                File.savePngToStream

        let result = ShadeCommand.run options generateTile saveTile
        test <@ result |> isOk @>
