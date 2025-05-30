﻿[<RequireQualifiedAccess>]
module Demeton.Commands.TileShadeCommand

open System
open System.Threading.Tasks
open CommandLine
open CommandLine.Common
open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Demeton.Geometry.Common
open Demeton.Projections
open Demeton.Projections.Common
open Demeton.Projections.PROJParsing
open Demeton.Aw3d.Types
open Demeton.Aw3d.Funcs
open Demeton.Shaders
open Demeton.Shaders.Types
open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.WaterBodies.DataSources
open Demeton.Shaders.WaterBodies.WaterBodiesShaders
open FileSys
open Png
open Png.Types
open Raster

type MapScaleOrPixelSize =
    | MapScaleOf of float
    | PixelSizeOf of float

type Options =
    { TileWidth: int
      TileHeight: int
      TileCenter: LonLat
      HgtSize: int
      MapScale: MapScaleOrPixelSize option
      Dpi: float
      LambertHillshadingIntensity: float
      IgorHillshadingIntensity: float
      SlopeShadingIntensity: float
      // In radians
      SunAzimuth: float
      // In radians
      SunAltitude: float
      WaterBodiesColor: Rgba8Bit.ArgbColor
      LocalCacheDir: string
      OutputDir: string
      OutputFileName: string }

[<Literal>]
let TileWidthParameter = "tile-width"

[<Literal>]
let TileHeightParameter = "tile-height"

[<Literal>]
let CenterLongitudeParameter = "center-longitude"

[<Literal>]
let CenterLatitudeParameter = "center-latitude"

[<Literal>]
let HgtSizeParameter = "hgt-size"

[<Literal>]
let PixelSizeParameter = "pixel-size"

[<Literal>]
let MapScaleParameter = "map-scale"

[<Literal>]
let DpiParameter = "dpi"

[<Literal>]
let LambertHillshadingIntensityParameter = "lambert-hillshading-intensity"

[<Literal>]
let IgorHillshadingIntensityParameter = "igor-hillshading-intensity"

[<Literal>]
let SlopeShadingIntensityParameter = "slope-shading-intensity"

[<Literal>]
let SunAzimuthParameter = "sun-azimuth"

[<Literal>]
let SunAltitudeParameter = "sun-altitude"

[<Literal>]
let WaterBodiesColorParameter = "water-color"

[<Literal>]
let LocalCacheDirParameter = "local-cache-dir"

[<Literal>]
let OutputDirParameter = "output-dir"

[<Literal>]
let OutputFileNameParameter = "output-file"

[<Literal>]
let DefaultLocalCacheDir = "cache"

[<Literal>]
let DefaultOutputDir = "output"

[<Literal>]
let DefaultOutputFileName = "tile.png"

[<Literal>]
let DefaultDpi = 254.

[<Literal>]
let Aw3dDataSourceKey = "aw3d"

[<Literal>]
let Aw3dWaterBodiesDataSourceKey = "aw3d-water-bodies"

let defaultWaterBodiesColor = "#49C8FF" |> Rgba8Bit.parseColorHexValue

let parseColorParameter value =
    match Rgba8Bit.tryParseColorHexValue value with
    | Ok color -> OkValue color
    | Error error -> InvalidValue error.Message

let supportedParameters: CommandParameter[] =
    [| Arg.build TileWidthParameter
       |> Arg.desc "The width of the tile (in pixels)."
       |> Arg.asPositiveInt
       |> Arg.toPar

       Arg.build TileHeightParameter
       |> Arg.desc "The height of the tile (in pixels)."
       |> Arg.asPositiveInt
       |> Arg.toPar

       Arg.build CenterLongitudeParameter
       |> Arg.desc "The longitude of the center of the tile."
       |> Arg.asFloatWithMin -180
       |> Arg.toPar

       Arg.build CenterLatitudeParameter
       |> Arg.desc "The latitude of the center of the tile."
       |> Arg.asFloatWithMin -90
       |> Arg.toPar

       Option.build HgtSizeParameter
       |> Option.desc
           "The width/height of the SRTM HGT tiles that will be generated during processing (default is 3600)."
       |> Option.asPositiveInt
       |> Option.defaultValue 3600
       |> Option.toPar

       Option.build PixelSizeParameter
       |> Option.desc "The size of the pixel (in meters)."
       |> Option.asPositiveFloat
       |> Option.defaultValue 20
       |> Option.toPar

       Option.build MapScaleParameter
       |> Option.desc "The map scale."
       |> Option.asPositiveFloat
       |> Option.toPar

       Option.build DpiParameter
       |> Option.desc "The DPI of the output image (the default is 254 DPI)."
       |> Option.asPositiveFloat
       |> Option.defaultValue DefaultDpi
       |> Option.toPar

       Option.build LambertHillshadingIntensityParameter
       |> Option.desc
           "The intensity of the Lambert hillshading (a non-negative float value, default is 1.0)."
       |> Option.asNonNegativeFloat
       |> Option.defaultValue 1.
       |> Option.toPar

       Option.build IgorHillshadingIntensityParameter
       |> Option.desc
           "The intensity of the Igor hillshading (a non-negative float value, default is 1.0)."
       |> Option.asNonNegativeFloat
       |> Option.defaultValue 1.
       |> Option.toPar

       Option.build SlopeShadingIntensityParameter
       |> Option.desc
           "The intensity of the slope shading (a non-negative float value, default is 1.0)."
       |> Option.asNonNegativeFloat
       |> Option.defaultValue 1.
       |> Option.toPar

       Option.build SunAzimuthParameter
       |> Option.desc
           "The azimuth of the sun in degrees, 0° representing north (default is 315°, northwest)."
       |> Option.defaultValue IgorHillshader.DefaultSunAzimuth
       |> Option.asFloat
       |> Option.toPar

       Option.build SunAltitudeParameter
       |> Option.desc
           "The altitude of the sun in degrees, 0° representing the horizon (default is 45°)."
       |> Option.defaultValue LambertHillshader.DefaultSunAltitude
       |> Option.asFloat
       |> Option.toPar

       Option.build WaterBodiesColorParameter
       |> Option.desc "The color of the water bodies."
       |> Option.example "#49C8FF" "uses a light blue color for water bodies"
       |> Option.parser parseColorParameter
       |> Option.defaultValue defaultWaterBodiesColor
       |> Option.toPar

       Option.build LocalCacheDirParameter
       |> Option.desc
           "The path to the local DEM cache directory. The directory will be created if it does not exist yet."
       |> Option.asDirectory
       |> Option.defaultValue DefaultLocalCacheDir
       |> Option.toPar

       Option.build OutputDirParameter
       |> Option.desc
           "The path to the directory where the raster files will be generated. The directory will be created if it does not exist yet."
       |> Option.asDirectory
       |> Option.defaultValue DefaultOutputDir
       |> Option.toPar

       Option.build OutputFileNameParameter
       |> Option.desc "The name of the output file (default is 'tile.png')."
       |> Option.asFileName
       |> Option.defaultValue DefaultOutputFileName
       |> Option.toPar |]

let fillOptions parsedParameters =
    let defaultOptions =
        { TileWidth = 100
          TileHeight = 100
          TileCenter = (0., 0.)
          HgtSize = 3600
          MapScale = None
          Dpi = DefaultDpi
          LambertHillshadingIntensity = 1.
          IgorHillshadingIntensity = 1.
          SlopeShadingIntensity = 1.
          SunAzimuth = IgorHillshader.DefaultSunAzimuth |> degToRad
          SunAltitude = LambertHillshader.DefaultSunAltitude |> degToRad
          WaterBodiesColor = defaultWaterBodiesColor
          LocalCacheDir = DefaultLocalCacheDir
          OutputDir = DefaultOutputDir
          OutputFileName = DefaultOutputFileName }

    let processParameter options parameter =
        match parameter with
        | ParsedArg { Name = TileWidthParameter
                      Value = value } ->
            { options with
                TileWidth = value :?> int }
        | ParsedArg { Name = TileHeightParameter
                      Value = value } ->
            { options with
                TileHeight = value :?> int }
        | ParsedArg { Name = CenterLongitudeParameter
                      Value = value } ->
            let lon = value :?> float

            { options with
                TileCenter = (lon, options.TileCenter |> snd) }
        | ParsedArg { Name = CenterLatitudeParameter
                      Value = value } ->
            let lat = value :?> float

            { options with
                TileCenter = (options.TileCenter |> fst, lat) }
        | ParsedOption { Name = HgtSizeParameter
                         Value = value } ->
            { options with HgtSize = value :?> int }
        | ParsedOption { Name = PixelSizeParameter
                         Value = value } ->
            { options with
                MapScale = Some(value :?> float |> PixelSizeOf) }
        | ParsedOption { Name = MapScaleParameter
                         Value = value } ->
            { options with
                MapScale = Some(value :?> float |> MapScaleOf) }
        | ParsedOption { Name = DpiParameter; Value = value } ->
            { options with Dpi = value :?> float }
        | ParsedOption { Name = LambertHillshadingIntensityParameter
                         Value = value } ->
            { options with
                LambertHillshadingIntensity = value :?> float }
        | ParsedOption { Name = IgorHillshadingIntensityParameter
                         Value = value } ->
            { options with
                IgorHillshadingIntensity = value :?> float }
        | ParsedOption { Name = SlopeShadingIntensityParameter
                         Value = value } ->
            { options with
                SlopeShadingIntensity = value :?> float }
        | ParsedOption { Name = SunAzimuthParameter
                         Value = value } ->
            { options with
                SunAzimuth = value :?> float |> degToRad }
        | ParsedOption { Name = SunAltitudeParameter
                         Value = value } ->
            { options with
                SunAltitude = value :?> float |> degToRad }
        | ParsedOption { Name = WaterBodiesColorParameter
                         Value = value } ->
            { options with
                WaterBodiesColor = value :?> Rgba8Bit.ArgbColor }
        | ParsedOption { Name = LocalCacheDirParameter
                         Value = value } ->
            { options with
                LocalCacheDir = value :?> string }
        | ParsedOption { Name = OutputDirParameter
                         Value = value } ->
            { options with
                OutputDir = value :?> string }
        | ParsedOption { Name = OutputFileNameParameter
                         Value = value } ->
            { options with
                OutputFileName = value :?> string }
        | _ -> invalidOp "Unrecognized parameter."

    let filledOptions =
        parsedParameters |> List.fold processParameter defaultOptions

    filledOptions



[<Literal>]
let StepNameWaterBodiesFromDem = "water-bodies-from-dem"

let constructShadingPipeline options =
    let solidBackgroundStepParameters: SolidBackground.Parameters =
        { BackgroundColor = Rgba8Bit.parseColorHexValue "#FFFFFF" }

    let solidBackgroundStep =
        ShadingStep.SolidBackground solidBackgroundStepParameters

    let igorHillshadingStep =
        ShadingStep.IgorHillshading
            { SunAzimuth = options.SunAzimuth
              ShadingColor = 0u
              Intensity = options.IgorHillshadingIntensity
              DataSourceKey = Aw3dWaterBodiesDataSourceKey }

    let lambertHillshadingStep =
        ShadingStep.LambertHillshading
            { SunAzimuth = options.SunAzimuth
              SunAltitude = options.SunAltitude
              ShadingColor = 0u
              Intensity = options.LambertHillshadingIntensity
              DataSourceKey = Aw3dWaterBodiesDataSourceKey }

    let slopeShadingStep =
        ShadingStep.SlopeShading
            { HorizontalColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 0uy
              VerticalColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 255uy
              Intensity = options.SlopeShadingIntensity
              DataSourceKey = Aw3dWaterBodiesDataSourceKey }

    let hillshadingStep1 =
        Compositing(
            lambertHillshadingStep,
            slopeShadingStep,
            CompositingFuncIdAlphaDarken
        )

    let hillshadingStep2 =
        Compositing(
            hillshadingStep1,
            igorHillshadingStep,
            CompositingFuncIdAlphaDarken
        )

    let waterBodiesStep = CustomShading StepNameWaterBodiesFromDem

    let hillAndWaterStep =
        Compositing(hillshadingStep2, waterBodiesStep, CompositingFuncIdOver)

    let rootShadingStep =
        Compositing(
            solidBackgroundStep,
            hillAndWaterStep,
            CompositingFuncIdOver
        )

    rootShadingStep

let createProjection options =
    let mapScaleValue =
        match options.MapScale with
        | Some(MapScaleOf mapScale) -> mapScale
        | Some(PixelSizeOf pixelSize) ->
            (pixelSize * options.Dpi) / MetersPerInch
        | None ->
            raise (
                NotImplementedException(
                    "Pixel size support not implemented yet"
                )
            )

    Log.info $"Using map scale of 1 : %f{Math.Round(mapScaleValue)}"

    let mapScale: MapScale =
        { MapScale = mapScaleValue
          Dpi = options.Dpi }

    let centerLon, centerLat = options.TileCenter

    // https://desktop.arcgis.com/en/arcmap/latest/map/projections/lambert-conformal-conic.htm
    let projectionParameters: LambertConformalConic.Parameters =
        { X0 = 0
          Y0 = 0
          Lon0 = centerLon
          Lat0 = centerLat
          // todo sometime 50: we set standard parallels to be the same as the center, for now
          Lat1 = centerLat
          Lat2 = centerLat
          K0 = 1.
          Ellipsoid = WGS84 }

    Factory.createMapProjection
        (LambertConformalConic projectionParameters)
        mapScale


let worldCoverWaterBodiesFromDemShader
    waterBodiesFromDemHeightsArrayDataSourceKey
    demTileSize
    waterColor
    debugMode
    : RasterShader =
    fun dataSources demLevel heightsArrayTargetArea imageData forward inverse ->
        let cellsPerDegree = cellsPerDegree demTileSize demLevel

        let waterBodiesFromDemHeightsArray =
            dataSources.FetchDataSource(
                waterBodiesFromDemHeightsArrayDataSourceKey
            )
            :?> HeightsArray

        let targetAreaWidth = heightsArrayTargetArea.Width

        let noWaterColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 0uy

        let processRasterLine y =
            for x in
                heightsArrayTargetArea.MinX .. (heightsArrayTargetArea.MaxX - 1) do
                let rasterValue =
                    waterBodiesFromDemHeightsArray
                    |> valueForProjectedPixel x y cellsPerDegree inverse

                let pixelValue =
                    match rasterValue with
                    | Some height when height % 2s = 1s -> waterColor
                    | _ -> noWaterColor

                Rgba8Bit.setPixelAt
                    imageData
                    targetAreaWidth
                    (x - heightsArrayTargetArea.MinX)
                    // we flip the Y coordinate since DEM heights array
                    // is flipped vertically compared to the bitmap
                    (heightsArrayTargetArea.MaxY - y - 1)
                    pixelValue

        Parallel.For(
            heightsArrayTargetArea.MinY,
            heightsArrayTargetArea.MaxY,
            processRasterLine
        )
        |> ignore


let createShaderFunction demTileSize waterColor debugMode shaderFunctionName =
    match shaderFunctionName with
    | StepNameWaterBodiesFromDem ->
        worldCoverWaterBodiesFromDemShader
            Aw3dWaterBodiesDataSourceKey
            demTileSize
            waterColor
            debugMode
    | StepNameWaterBodies ->
        worldCoverWaterBodiesShader
            WaterBodiesHeightsArrayDataSourceKey
            WaterBodiesColoredListDataSourceKey
            waterColor
            debugMode
    | StepNameWaterBodiesOutline ->
        worldCoverWaterBodiesOutlineShader
            WaterBodiesHeightsArrayDataSourceKey
            WaterBodiesOutlinesDataSourceKey
    | _ -> failwithf $"Unknown shader function name: %s{shaderFunctionName}"


/// <summary>
/// Calculates the geographic minimum bounding rectangle for the given
/// output bitmap size and the map projection used.
/// </summary>
/// <remarks>
/// The function calculates the MBR by de-projecting the four edges of the
/// bitmap to geographic coordinates and then calculating the MBR of those
/// geographic points.
/// </remarks>
/// <param name="bitmapRect">The rectangle representing the output bitmap.</param>
/// <param name="projection">The map projection used.</param>
let calculateGeoAreaMbr (bitmapRect: Rect) projection =
    let centerX = bitmapRect.MinX + bitmapRect.Width / 2
    let centerY = bitmapRect.MinY + bitmapRect.Height / 2

    let tileBoundingPoints: (int * int)[] =
        [| (bitmapRect.MinX, bitmapRect.MinY)
           (centerX, bitmapRect.MinY)
           (bitmapRect.MaxX, bitmapRect.MinY)
           (bitmapRect.MaxX, centerY)
           (bitmapRect.MaxX, bitmapRect.MaxY)
           (centerX, bitmapRect.MaxY)
           (bitmapRect.MinX, bitmapRect.MaxY)
           (bitmapRect.MinX, centerY) |]

    let tileBoundingGeoPoints =
        tileBoundingPoints
        |> Array.map (fun (x, y) -> projection.Invert x y)
        |> Array.choose id
        |> Array.map (fun (lon, lat) -> (radToDeg lon, radToDeg lat))

    if
        tileBoundingGeoPoints |> Array.length = (tileBoundingPoints
                                                 |> Array.length)
    then
        Demeton.Geometry.Bounds.mbrOf tileBoundingGeoPoints
        |> lonLatBoundsFromBounds
        |> Ok
    else
        Result.Error "Some of the tile bounding points could not be projected."


let fetchAw3dHeightsArray mapProjection cacheDir demLevel coverageArea =
    let coveragePoints =
        [ (coverageArea.MinLon, coverageArea.MinLat)
          (coverageArea.MaxLon, coverageArea.MinLat)
          (coverageArea.MinLon, coverageArea.MaxLat)
          (coverageArea.MaxLon, coverageArea.MaxLat) ]

    let tileDownloadingResult = ensureAw3dTiles cacheDir coverageArea

    match tileDownloadingResult with
    | Ok tilesIds ->
        let tilesHeightsArrays =
            tilesIds |> Seq.map (readAw3dTile cacheDir) |> Seq.toList

        // calculate mergedArrayBounds for the given area
        let projectedCoveragePoints =
            coveragePoints
            |> List.map (fun (lon, lat) ->
                mapProjection.Proj (lon |> degToRad) (lat |> degToRad))
            |> List.choose id

        let deprojectedCoveragePoints =
            projectedCoveragePoints
            |> List.map (fun (x, y) -> mapProjection.Invert x y)
            |> List.choose id

        // todo 5: how to deal with half-width tiles here?
        let cellsPerDegree = Aw3dDefaultTileWidth

        // now convert lon, lat to DEM coordinates
        let coveragePointsInDemCoords =
            deprojectedCoveragePoints
            |> List.map (fun (lon, lat) ->
                let cellX =
                    lon |> radToDeg |> longitudeToCellX (float cellsPerDegree)

                let cellY =
                    lat |> radToDeg |> latitudeToCellY (float cellsPerDegree)

                (cellX, cellY))

        let demMbr = Demeton.Geometry.Bounds.mbrOf coveragePointsInDemCoords

        let mergedArrayBounds =
            Rect.asMinMax
                (demMbr.MinX |> floor |> int)
                (demMbr.MinY |> floor |> int)
                (demMbr.MaxX |> ceil |> int)
                (demMbr.MaxY |> ceil |> int)

        // A buffer around the DEM MBR, so we don't end up outside the array
        // when we calculate the heights for hillshading.
        // Because of map projection issues, we (currently) take around 10% of
        // the larger side of the merged array bounds.
        // NOTE(!!) that this is only a temporary solution - those 10% could
        // exceed the actual downloaded DEM tiles area, and we would end up with
        // holes in the data. So buffered area should be calculated earlier in
        // the process and then just provided to fetchAw3dHeightsArray() to
        // fetch a bigger area (without the buffer calculation here).
        let safetyBuffer =
            Math.Max(mergedArrayBounds.Width, mergedArrayBounds.Height)
            |> float
            |> (*) 0.1
            |> int

        let mergedArrayBounds = mergedArrayBounds |> inflate safetyBuffer

        merge mergedArrayBounds tilesHeightsArrays |> Result.Ok
    | Error message -> Result.Error message


let saveTileFile
    (ensureDirectoryExists: DirectoryExistsEnsurer)
    (openFileToWrite: FileWriter)
    (writePngToStream: File.PngStreamWriter)
    (tileRect: Rect)
    outputDir
    outputFileName
    imageData
    =
    ensureDirectoryExists outputDir |> ignore

    let tilePngFileName = outputDir |> Pth.combine outputFileName

    openFileToWrite tilePngFileName
    |> Result.map (fun stream ->
        let ihdr: IhdrData =
            { Width = tileRect.Width
              Height = tileRect.Height
              BitDepth = PngBitDepth.BitDepth8
              ColorType = PngColorType.RgbAlpha
              InterlaceMethod = PngInterlaceMethod.NoInterlace }

        stream |> writePngToStream ihdr imageData |> closeStream

        Log.info $"Saved the tile to %s{tilePngFileName}"

        tilePngFileName)
    |> Result.mapError fileSysErrorMessage

let fetchDemWithWaterBodies
    demTileSize
    mapProjection
    cacheDir
    level
    coverageArea
    =
    Log.info "Ensuring all needed DEM tiles are there..."

    let coveragePoints =
        [ (coverageArea.MinLon, coverageArea.MinLat)
          (coverageArea.MaxLon, coverageArea.MinLat)
          (coverageArea.MinLon, coverageArea.MaxLat)
          (coverageArea.MaxLon, coverageArea.MaxLat) ]

    Log.info
        "Geo area needed: minLon: %f, minLat: %f, maxLon: %f, maxLat: %f"
        coverageArea.MinLon
        coverageArea.MinLat
        coverageArea.MaxLon
        coverageArea.MaxLat

    let tilesIds =
        coverageArea |> boundsToTiles demTileSize DemLevel.Level0 |> Seq.toList

    let tilesHeightsArrays =
        tilesIds
        |> Seq.map (
            DemWithWaterBodiesCommand.ensureXthFile cacheDir demTileSize
        )
        |> Seq.toList
        |> List.choose Some
        |> List.map Option.get

    // calculate mergedArrayBounds for the given area
    let projectedCoveragePoints =
        coveragePoints
        |> List.map (fun (lon, lat) ->
            mapProjection.Proj (lon |> degToRad) (lat |> degToRad))
        |> List.choose id

    let deprojectedCoveragePoints =
        projectedCoveragePoints
        |> List.map (fun (x, y) -> mapProjection.Invert x y)
        |> List.choose id

    // now convert lon, lat to DEM coordinates
    let coveragePointsInDemCoords =
        deprojectedCoveragePoints
        |> List.map (fun (lon, lat) ->
            let cellX = lon |> radToDeg |> longitudeToCellX (float demTileSize)
            let cellY = lat |> radToDeg |> latitudeToCellY (float demTileSize)

            (cellX, cellY))

    let demMbr = Demeton.Geometry.Bounds.mbrOf coveragePointsInDemCoords

    let mergedArrayBounds =
        Rect.asMinMax
            (demMbr.MinX |> floor |> int)
            (demMbr.MinY |> floor |> int)
            (demMbr.MaxX |> ceil |> int)
            (demMbr.MaxY |> ceil |> int)

    // A buffer around the DEM MBR, so we don't end up outside the array
    // when we calculate the heights for hillshading.
    // Because of map projection issues, we (currently) take around 10% of
    // the larger side of the merged array bounds.
    // NOTE(!!) that this is only a temporary solution - those 10% could
    // exceed the actual downloaded DEM tiles area, and we would end up with
    // holes in the data. So buffered area should be calculated earlier in
    // the process and then just provided to fetchDemWithWaterBodies() to
    // fetch a bigger area (without the buffer calculation here).
    let safetyBuffer =
        Math.Max(mergedArrayBounds.Width, mergedArrayBounds.Height)
        |> float
        |> (*) 0.1
        |> int

    let mergedArrayBounds = mergedArrayBounds |> inflate safetyBuffer

    merge mergedArrayBounds tilesHeightsArrays |> Result.Ok


let run (options: Options) : Result<unit, string> =
    let cacheDir = options.LocalCacheDir
    let srtmLevel: DemLevel = { Value = 0 }

    let rootShadingStep = constructShadingPipeline options

    // the center of the bitmap rect should represent the center specified in
    // the options
    let bitmapRect =
        { MinX = -options.TileWidth / 2
          MinY = -options.TileHeight / 2
          Width = options.TileWidth
          Height = options.TileHeight }

    // inflate the bitmap rect by one pixel on each side, so it is safe for
    // various hillshading neighbor calculations
    let bitmapRectSafe = bitmapRect |> inflate 1

    match createProjection options with
    | Ok mapProjection ->
        match calculateGeoAreaMbr bitmapRect mapProjection with
        | Ok coverageArea ->
            let waterBodiesDebugMode = false

            ShadeCommand.generateShadedRasterTile
                options.HgtSize
                [| fun level coverageArea dataSources ->
                       fetchDemWithWaterBodies
                           options.HgtSize
                           mapProjection
                           cacheDir
                           level
                           coverageArea
                       |> heightsArrayResultToShadingDataSource
                           Aw3dWaterBodiesDataSourceKey
                           (Ok dataSources) |]
                (createShaderFunction
                    options.HgtSize
                    options.WaterBodiesColor
                    waterBodiesDebugMode)
                srtmLevel
                bitmapRect
                rootShadingStep
                mapProjection
            |> Result.bind (fun imageData ->
                match imageData with
                | Some imageData ->
                    Log.info "Saving the tile..."

                    saveTileFile
                        ensureDirectoryExists
                        openFileToWrite
                        File.savePngToStream
                        bitmapRect
                        options.OutputDir
                        options.OutputFileName
                        imageData
                    |> ignore

                    Ok()
                | None -> Error "No image data generated.")
        | Error message -> Result.Error message
    | Error message -> Result.Error message
