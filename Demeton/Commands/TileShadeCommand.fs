[<RequireQualifiedAccess>]
module Demeton.Commands.TileShadeCommand

open System
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
open Demeton.WorldCover.Types
open Demeton.WorldCover.Fetch
open Demeton.Shaders
open Demeton.Shaders.Types
open Demeton.Shaders.WaterBodies.WaterBodiesShading
open Demeton.Shaders.WaterBodies.WaterBodiesShaders
open FileSys
open Png
open Png.Types
open Raster
open Demeton.Shaders.Pipeline.Common

type Options =
    { TileWidth: int
      TileHeight: int
      TileCenter: LonLat
      PixelSize: float option
      MapScale: float option
      Dpi: float
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
let PixelSizeParameter = "pixel-size"

[<Literal>]
let MapScaleParameter = "map-scale"

[<Literal>]
let DpiParameter = "dpi"

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
       |> Option.desc "The name of the output file (default it 'tile.png')."
       |> Option.asFileName
       |> Option.defaultValue DefaultOutputFileName
       |> Option.toPar |]

let fillOptions parsedParameters =
    let defaultOptions =
        { TileWidth = 100
          TileHeight = 100
          TileCenter = (0., 0.)
          PixelSize = None
          MapScale = None
          Dpi = DefaultDpi
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
        | ParsedOption { Name = PixelSizeParameter
                         Value = value } ->
            { options with
                PixelSize = Some(value :?> float) }
        | ParsedOption { Name = MapScaleParameter
                         Value = value } ->
            { options with
                MapScale = Some(value :?> float) }
        | ParsedOption { Name = DpiParameter; Value = value } ->
            { options with Dpi = value :?> float }
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

    let filledOptions =
        match filledOptions.PixelSize, filledOptions.MapScale with
        | Some _, Some _ ->
            invalidOp "Cannot specify both pixel size and map scale."
        | None, None ->
            { filledOptions with
                PixelSize = Some 20. }
        | _ -> filledOptions

    filledOptions

let createProjection options =
    let mapScaleValue =
        match options.MapScale with
        | Some mapScale -> mapScale
        | None ->
            raise (
                NotImplementedException(
                    "Pixel size support not implemented yet"
                )
            )

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
          // todo 50: we set standard parallels to be the same as the center, for now
          Lat1 = centerLat
          Lat2 = centerLat
          K0 = 1.
          Ellipsoid = WGS84 }

    Factory.createMapProjection
        (LambertConformalConic projectionParameters)
        mapScale


[<Literal>]
let WaterBodiesHeightsArrayDataSourceKey = "waterBodiesRaster"

[<Literal>]
let WaterBodiesColoredListDataSourceKey = "waterBodiesColoredList"

[<Literal>]
let WaterBodiesOutlinesDataSourceKey = "waterBodiesOutlines"

// todo 0: move createShaderFunction to the command
let createShaderFunction shaderFunctionName =
    match shaderFunctionName with
    | StepNameWaterBodies ->
        worldCoverWaterBodiesShader
            WaterBodiesHeightsArrayDataSourceKey
            WaterBodiesColoredListDataSourceKey
    | StepNameWaterBodiesOutline ->
        worldCoverWaterBodiesOutlineShader
            WaterBodiesHeightsArrayDataSourceKey
            WaterBodiesOutlinesDataSourceKey
    | _ ->
        failwithf
            $"Unknown shader function name: %s{shaderFunctionName}"



let calculateGeoAreaMbr options projection =
    let tileBoundingPoints: (int * int)[] =
        [| (-1, -1); (1, -1); (1, 1); (-1, 1) |]

    let halfWidth = options.TileWidth / 2
    let halfHeight = options.TileHeight / 2

    let tileBoundingGeoPoints =
        tileBoundingPoints
        |> Array.map (fun (x, y) ->
            let x = x * halfWidth
            let y = y * halfHeight

            projection.Invert x y)
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

        let cellsPerDegree = Aw3dTileSize

        // now convert lon, lat to DEM coordinates
        let coveragePointsInDemCoords =
            deprojectedCoveragePoints
            |> List.map (fun (lon, lat) ->
                let cellX = lon |> radToDeg |> longitudeToCellX cellsPerDegree
                let cellY = lat |> radToDeg |> latitudeToCellY cellsPerDegree
                (cellX, cellY))

        let demMbr = Demeton.Geometry.Bounds.mbrOf coveragePointsInDemCoords

        // a buffer around the DEM MBR so we don't end up outside of the array
        // when we calculate the heights
        let safetyBuffer = 5

        let mergedArrayBounds =
            Rect.asMinMax
                ((demMbr.MinX |> floor |> int) - safetyBuffer)
                ((demMbr.MinY |> floor |> int) - safetyBuffer)
                ((demMbr.MaxX |> ceil |> int) + safetyBuffer)
                ((demMbr.MaxY |> ceil |> int) + safetyBuffer)

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


let run (options: Options) : Result<unit, string> =
    let cacheDir = options.LocalCacheDir
    let srtmLevel: DemLevel = { Value = 0 }

    // the center of the raster rect should represent the center specified in
    // the options
    let tileRect =
        { MinX = -options.TileWidth / 2
          MinY = -options.TileHeight / 2
          Width = options.TileWidth
          Height = options.TileHeight }

    let solidBackgroundStepParameters: SolidBackground.Parameters =
        { BackgroundColor = Rgba8Bit.parseColorHexValue "#FFFFFF" }

    let solidBackgroundStep =
        ShadingStep.SolidBackground solidBackgroundStepParameters

    let igorHillshadingStep =
        ShadingStep.IgorHillshading
            { SunAzimuth = options.SunAzimuth
              ShadingColor = 0u
              Intensity = options.IgorHillshadingIntensity
              DataSourceKey = DefaultDataSourceKey }

    let lambertHillshadingStep =
        ShadingStep.LambertHillshading
            { SunAzimuth = options.SunAzimuth
              SunAltitude = options.SunAltitude
              ShadingColor = 0u
              Intensity = options.IgorHillshadingIntensity
              DataSourceKey = DefaultDataSourceKey }

    let slopeShadingStep =
        ShadingStep.SlopeShading
            { HorizontalColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 0uy
              VerticalColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 255uy
              Intensity = options.SlopeShadingIntensity
              DataSourceKey = DefaultDataSourceKey }

    let hillshadingStep =
        Compositing(
            lambertHillshadingStep,
            slopeShadingStep,
            CompositingFuncIdAlphaDarken
        )

    let rootShadingStep =
        Compositing(solidBackgroundStep, hillshadingStep, CompositingFuncIdOver)

    match createProjection options with
    | Ok mapProjection ->
        match calculateGeoAreaMbr options mapProjection with
        | Ok coverageArea ->
            ShadeCommand.generateShadedRasterTile
                [| fun level coverageArea dataSources ->
                       fetchAw3dHeightsArray
                           mapProjection
                           cacheDir
                           level
                           coverageArea
                       |> heightsArrayResultToShadingDataSource
                           "aw3d"
                           (Ok dataSources) |]
                createShadingFuncById
                srtmLevel
                tileRect
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
                        tileRect
                        options.OutputDir
                        options.OutputFileName
                        imageData
                    |> ignore

                    Ok()
                | None -> Error "No image data generated.")
        | Error message -> Result.Error message
    | Error message -> Result.Error message
