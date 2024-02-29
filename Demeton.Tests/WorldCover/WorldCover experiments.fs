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

// todo 5: this function should accept the SrtmTileId that needs to be read
//   and then find the required TIFF tiles to fill the HeightsArray.
//   We can then test the result by implementing a custom shader that renders
//   just the water bodies. We will deal with mixing this together with AW3D
//   later.


let readWorldCoverRaster
    (fileName: string)
    (raster_file_tile_coords: SrtmTileCoords)
    (requested_tile_coords: SrtmTileCoords)
    : DemHeight[] =
    use tiff = Tiff.Open(fileName, "r")

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

    let unreducedHeightsArray: DemHeight[] =
        Array.zeroCreate (pixelsPerSrtmTile * pixelsPerSrtmTile)

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

    let reductionFactor = pixelsPerSrtmTile / 3600

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

            // todo 0: copy the contents of the buffer into the heightsArray

            // todo 10: not exactly optimal
            for i in 0 .. tiffTileBufferSize - 1 do
                let value = tiffTileBuffer.[i]
                let x = tiffTileX + i % tiffTileWidth
                let y = tiffTileY + i / tiffTileWidth
                let index = y * 3600 + x
                unreducedHeightsArray.[index] <- int16 value

            // now reduce the heightsArray to the SRTM size

            // todo 20: not really a good downsampling since it just takes the
            //   first value in each reductionFactor x reductionFactor block
            for y in 0..3599 do
                for x in 0..3599 do
                    let srtmIndex = y * 3600 + x

                    let unreducedIndex =
                        (y * reductionFactor) * pixelsPerSrtmTile
                        + (x * reductionFactor)

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

let waterBodiesShader: RasterShader =
    fun heightsArray srtmLevel tileRect imageData inverse ->
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
                    int (Math.Round(lonDeg |> longitudeToCellX cellsPerDegree))

                let globalSrtmY =
                    latDeg
                    |> latitudeToCellY cellsPerDegree
                    |> Math.Round
                    |> int

                heightsArray.heightAt (globalSrtmX, globalSrtmY) |> Some

        let processRasterLine y =
            for x in tileRect.MinX .. (tileRect.MaxX - 1) do
                let rasterValue = valueForTilePixel x y

                let pixelValue =
                    match rasterValue with
                    | Some 80s -> Rgba8Bit.rgbColor 0uy 0uy 255uy
                    | _ -> Rgba8Bit.rgbColor 255uy 255uy 255uy

                Rgba8Bit.setPixelAt
                    imageData
                    tileWidth
                    (x - tileRect.MinX)
                    (y - tileRect.MinY)
                    pixelValue

        Parallel.For(tileRect.MinY, tileRect.MaxY, processRasterLine) |> ignore


[<Fact>]
let ``Load WorldCover file into a DemHeight`` () =
    let demHeight =
        readWorldCoverRaster
            @"Samples\ESA_WorldCover_10m_2021_v200_N45E006_Map.tif"
            { Lon = { Value = 6 }
              Lat = { Value = -45 } }
            { Lon = { Value = 7 }
              Lat = { Value = -46 } }

    let tileSize = 3600

    let tileId = parseTileName "N46E007"
    let cellMinX, cellMinY = tileMinCell tileSize tileId

    let fetchHeightsArray tileIds =
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

    let createShaderFunction _ = waterBodiesShader

    let generateTile =
        ShadeCommand.generateShadedRasterTile
            fetchHeightsArray
            createShaderFunction

    let saveTile =
        ShadeCommand.saveShadedRasterTile
            FileSys.ensureDirectoryExists
            FileSys.openFileToWrite
            File.savePngToStream

    let result = ShadeCommand.run options generateTile saveTile
    test <@ result |> isOk @>
