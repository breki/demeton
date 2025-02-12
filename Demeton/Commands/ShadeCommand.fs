﻿[<RequireQualifiedAccess>]
module Demeton.Commands.ShadeCommand

open CommandLine
open CommandLine.Common
open Demeton.Dem.Types
open Demeton.Shaders.Types
open Raster
open Demeton.Geometry
open Demeton.Geometry.Common
open Demeton.Projections.Common
open Demeton.Projections.Factory
open Demeton.Projections.MinLonLatDelta
open Demeton.Projections.PROJParsing
open Demeton.Shaders
open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.Pipeline.Parsing
open Demeton.Shaders.Pipeline.Building
open Demeton.Shaders.Pipeline.BuildingAspectShader
open Demeton.Shaders.Pipeline.BuildingElevationColoring
open Demeton.Shaders.Pipeline.BuildingIgorHillshading
open Demeton.Shaders.Pipeline.BuildingSlopeShader
open Demeton.Srtm
open FileSys
open Png.Types
open Text
open System

type Options =
    { CoveragePoints: LonLat list
      FilePrefix: string
      LocalCacheDir: string
      OutputDir: string
      SrtmDir: string
      TileSize: int
      RootShadingStep: ShadingStep
      MapScale: MapScale
      MapProjection: PROJProjection }

[<Literal>]
let CoveragePointsParameter = "coverage"

[<Literal>]
let DpiParameter = "dpi"

[<Literal>]
let FilePrefixParameter = "file-prefix"

[<Literal>]
let LocalCacheDirParameter = "local-cache-dir"

[<Literal>]
let MapScaleParameter = "map-scale"

[<Literal>]
let OutputDirParameter = "output-dir"

[<Literal>]
let ProjectionParameter = "proj"

[<Literal>]
let ShadingScriptParameter = "shading-script"

[<Literal>]
let SrtmDirParameter = "srtm-dir"

[<Literal>]
let TileSizeParameter = "tile-size"

[<Literal>]
let DefaultDpi = 300.

[<Literal>]
let DefaultFilePrefix = "shading"

[<Literal>]
let DefaultLocalCacheDir = "cache"

[<Literal>]
let DefaultMapScale = 50000.

[<Literal>]
let DefaultOutputDir = "output"

[<Literal>]
let DefaultSrtmDir = "srtm"

[<Literal>]
let DefaultTileSize = 1000

let parseCoverage value =
    let floatsListResult = TextParsers.parseFloatsList value

    match floatsListResult with
    | Error _ -> InvalidValue "it has to consist of a list of coordinates"
    | Ok floatsList ->
        match floatsList.Length with
        | l when l % 2 <> 0 ->
            InvalidValue "it has an odd number of coordinates"
        | l when l < 4 ->
            InvalidValue "it has to have at least two points specified"
        | _ ->
            let coveragePoints = floatsListToPoints floatsList
            OkValue coveragePoints

let parseProjSpecParameter value : OptionValueParsingResult =
    match parseProjSpecProjection value with
    | Ok value -> OkValue value
    | Error ProjectionNotSpecified ->
        InvalidValue
            "PROJ specification does not contain a map projection ('+proj') parameter"
    | Error(SpecParsingError error) ->
        InvalidValue(Environment.NewLine + error.Message)
    | Error(UnsupportedProjection projectionId) ->
        $"unsupported map projection '%s{projectionId}'" |> InvalidValue
    | Error(InvalidProjectionParameters message) ->
        InvalidValue(Environment.NewLine + message)

let registeredStepBuilders =
    dict
        [ ("aspect", aspectShaderStepBuilder)
          (StepNameElevationColoring, elevationColoringStepBuilder)
          (StepNameIgorHillshading, igorHillshadingStepBuilder)
          (StepNameSlopeShading, slopeShaderStepBuilder) ]

let parseShadingScriptOption: OptionValueParser =
    fun value ->
        match parseShadingScript value with
        | Ok parsedScript ->
            match buildShadingPipeline registeredStepBuilders parsedScript with
            | Ok rootStep -> OkValue rootStep
            | Error errorMessage ->
                buildString ()
                |> newLine
                |> append errorMessage
                |> toString
                |> InvalidValue
        | Error parsingError ->
            buildString ()
            |> newLine
            |> appendLine value
            |> appendLine $"%s{String(' ', parsingError.Location)}^"
            |> append parsingError.Message
            |> append "."
            |> toString
            |> InvalidValue

let supportedParameters: CommandParameter[] =
    [| Arg.build CoveragePointsParameter
       |> Arg.desc
           "A list of points to be covered. At least two points need to be specified."
       |> Arg.format "x1,y1,x2,y2..."
       |> Arg.example "5,43.3,16.6,48.4" "fetches (roughly) the whole Alps area"
       |> Arg.parser parseCoverage
       |> Arg.toPar

       Option.build DpiParameter
       |> Option.desc
           "The printing resolution required for the resulting raster image."
       |> Option.asPositiveFloat
       |> Option.defaultValue DefaultDpi
       |> Option.example
           "1200"
           "specifies the printing resolution of 1200 dots per inch"
       |> Option.toPar

       Option.build FilePrefixParameter
       |> Option.desc
           "The text used to prefix names of all generated image files."
       |> Option.asFileName
       |> Option.defaultValue DefaultFilePrefix
       |> Option.example
           "hillshade"
           "all generated image file names will start with 'hillshade', like 'hillshade-2-3.png"
       |> Option.toPar

       Option.build LocalCacheDirParameter
       |> Option.desc
           "The path to the local SRTM cache directory. The directory will be created if it does not exist yet."
       |> Option.asDirectory
       |> Option.defaultValue DefaultLocalCacheDir
       |> Option.toPar

       Option.build ProjectionParameter
       |> Option.desc "The map projection to use (PROJ specification text)."
       |> Option.defaultValue "Mercator projection"
       |> Option.parser parseProjSpecParameter
       // todo: add example once we have support for other projections
       //    |> Option.example "100000"  "the map scale of resulting raster image will be 1 : 100,000"
       |> Option.toPar

       Option.build MapScaleParameter
       |> Option.desc "The map scale needed for the resulting raster image."
       |> Option.asFloatWithMin 1.
       |> Option.defaultValue DefaultMapScale
       |> Option.example
           "100000"
           "the map scale of resulting raster image will be 1 : 100,000"
       |> Option.toPar

       Option.build OutputDirParameter
       |> Option.desc
           "The path to the directory where the raster files will be generated. The directory will be created if it does not exist yet."
       |> Option.asDirectory
       |> Option.defaultValue DefaultOutputDir
       |> Option.toPar

       Option.build ShadingScriptParameter
       |> Option.desc
           "The path to the directory where the raster files will be generated. The directory will be created if it does not exist yet."
       |> Option.parser parseShadingScriptOption
       |> Option.defaultValue "elevation coloring + hillshading"
       // todo: add example
       //|> Option.example "1200"  "specifies the printing resolution of 1200 dots per inch"
       |> Option.toPar

       Option.build SrtmDirParameter
       |> Option.desc
           "The path to the directory containing the original zipped SRTM HGT files."
       |> Option.asDirectory
       |> Option.defaultValue DefaultSrtmDir
       |> Option.toPar

       Option.build TileSizeParameter
       |> Option.desc (
           "The maximum width and height in pixels of an individual raster image tile. "
           + "If the image is larger than this size, it will be split into multiple tiles."
       )
       |> Option.asPositiveInt
       |> Option.defaultValue DefaultTileSize
       |> Option.example
           "1200"
           "specifies the printing resolution of 1200 dots per inch"
       |> Option.toPar |]


let fillOptions parsedParameters =
    let igorShaderParameters: IgorHillshader.ShaderParameters =
        IgorHillshader.defaultParameters

    let shadingPipeline =
        Compositing(
            ElevationColoring
                { ColorScale = ElevationColoring.colorScaleMaperitive
                  DataSourceKey = DefaultDataSourceKey },
            IgorHillshading igorShaderParameters,
            CompositingFuncIdOver
        )

    let defaultOptions =
        { CoveragePoints = []
          FilePrefix = DefaultFilePrefix
          LocalCacheDir = DefaultLocalCacheDir
          OutputDir = DefaultOutputDir
          SrtmDir = DefaultSrtmDir
          TileSize = DefaultTileSize
          RootShadingStep = shadingPipeline
          MapScale =
            { MapScale = DefaultMapScale
              Dpi = DefaultDpi }
          MapProjection =
            { Projection = Mercator
              IgnoredParameters = [] } }

    let processParameter options parameter =
        match parameter with
        | ParsedArg { Name = CoveragePointsParameter
                      Value = value } ->
            { options with
                CoveragePoints = value :?> LonLat list }
        | ParsedOption { Name = DpiParameter; Value = value } ->
            { options with
                MapScale =
                    { options.MapScale with
                        Dpi = value :?> float } }
        | ParsedOption { Name = FilePrefixParameter
                         Value = value } ->
            { options with
                FilePrefix = value :?> string }
        | ParsedOption { Name = LocalCacheDirParameter
                         Value = value } ->
            { options with
                LocalCacheDir = value :?> string }
        | ParsedOption { Name = MapScaleParameter
                         Value = value } ->
            { options with
                MapScale =
                    { options.MapScale with
                        MapScale = value :?> float } }
        | ParsedOption { Name = ProjectionParameter
                         Value = value } ->
            { options with
                MapProjection = value :?> PROJProjection }
        | ParsedOption { Name = OutputDirParameter
                         Value = value } ->
            { options with
                OutputDir = value :?> string }
        | ParsedOption { Name = ShadingScriptParameter
                         Value = value } ->
            { options with
                RootShadingStep = value :?> ShadingStep }
        | ParsedOption { Name = SrtmDirParameter
                         Value = value } ->
            { options with
                SrtmDir = value :?> string }
        | ParsedOption { Name = TileSizeParameter
                         Value = value } ->
            { options with
                TileSize = value :?> int }
        | _ -> invalidOp "Unrecognized parameter."

    parsedParameters |> List.fold processParameter defaultOptions


let splitIntoIntervals minValue maxValue intervalSize =
    let spaceLength = maxValue - minValue

    let intervalsRemainder =
        match spaceLength % intervalSize with
        | 0 -> 0
        | _ -> 1

    let intervalsCount = spaceLength / intervalSize + intervalsRemainder

    Seq.init intervalsCount (fun intervalIndex ->
        let intervalMinValue = minValue + intervalIndex * intervalSize
        let intervalMaxValue = min (intervalMinValue + intervalSize) maxValue
        (intervalIndex, intervalMinValue, intervalMaxValue))


/// <summary>
/// Based on the coverage points specified in the options, calculate the
/// minimum bounding rectangle of the points projected onto the map.
/// This effectively calculates the total raster area that needs to be shaded.
/// </summary>
let calculateRasterMbr mapProjection options =
    // project each coverage point
    let projectedPoints =
        options.CoveragePoints
        |> List.map (fun (lonDegrees, latDegrees) ->
            mapProjection.Proj (degToRad lonDegrees) (degToRad latDegrees))
        |> List.filter Option.isSome
        |> List.map (Option.get >> (fun (x, y) -> (x, y)))

    // calculate the minimum bounding rectangle of all the projected points
    let rasterMbr = Bounds.mbrOf projectedPoints

    // round off the raster so we work with integer coordinates
    Rect.asMinMax
        (int (floor rasterMbr.MinX))
        (int (floor rasterMbr.MinY))
        (int (ceil rasterMbr.MaxX))
        (int (ceil rasterMbr.MaxY))


/// <summary>
/// Based on the map projection and the raster MBR, calculate the DEM level
/// needed for the shading process.
/// </summary>
let calculateDemLevelNeeded tileSize mapProjection rasterMbr =
    minLonLatDelta rasterMbr mapProjection.Invert
    |> lonLatDeltaToDemLevel tileSize


/// <summary>
/// Calculates the list of raster tiles to generate based on the raster MBR and
/// options.
/// Also returns the maximum tile index (either on X or Y axes, whichever is
/// larger).
/// </summary>
let constructRasterTilesList options (rasterMbr: Rect) =
    let tilesToGenerate =
        [ for yIndex, tileMinY, tileMaxY in
              splitIntoIntervals rasterMbr.MinY rasterMbr.MaxY options.TileSize do
              for xIndex, tileMinX, tileMaxX in
                  splitIntoIntervals
                      rasterMbr.MinX
                      rasterMbr.MaxX
                      options.TileSize do
                  let rasterTileRect =
                      Rect.asMinMax tileMinX tileMinY tileMaxX tileMaxY

                  yield (xIndex, yIndex, rasterTileRect) ]

    let maxTileIndexX =
        tilesToGenerate |> List.map (fun (xIndex, _, _) -> xIndex) |> List.max

    let maxTileIndexY =
        tilesToGenerate |> List.map (fun (_, yIndex, _) -> yIndex) |> List.max

    let maxTileIndex = max maxTileIndexX maxTileIndexY

    Log.info
        "NOTE: The command will generate a total raster size of %dx%d pixels (%dx%d tiles)."
        rasterMbr.Width
        rasterMbr.Height
        (maxTileIndexX + 1)
        (maxTileIndexY + 1)

    tilesToGenerate, maxTileIndex


/// <summary>
/// A function that generates a shaded raster image.
/// </summary>
/// <remarks>
/// Given the DEM level, the tile rectangle, the root shading step, and the
/// map projection, the function should obtain the necessary geographical
/// information, run the shading pipeline, and return the raw image data of
/// the shaded raster.
/// </remarks>
/// <param name="demLevel">The DEM level to use.</param>
/// <param name="imageRect">The rectangle representing the image,
/// in the coordinates of the map projection.</param>
/// <param name="rootShadingStep">The root shading step to use.</param>
/// <param name="mapProjection">The map projection to use.</param>
/// <returns>
/// A result type that contains the raw image data of the shaded raster
/// if the generation is successful, or an error message if the generation
/// fails
/// </returns>
type ShadedRasterImageGenerator =
    DemLevel
        -> Rect
        -> ShadingStep
        -> MapProjection
        -> Result<RawImageData option, string>


/// <summary>
/// Returns a ShadedRasterImageGenerator that uses the given functions
/// to fetch the heights array and create the shading function.
/// </summary>
/// <param name="dataSourcesFetchers">
/// An array of functions that fetch the shading data sources for a given
/// geographical area at the specific DEM level.
/// </param>
/// <param name="createShaderFunction">
/// A function that creates a shading function for a given shading step.
/// </param>
/// <param name="tileSize">The size of the DEM tiles used.</param>
/// <param name="demLevel">The DEM level to use.</param>
/// <param name="imageRect">The rectangle representing the image,
/// in the coordinates of the map projection.</param>
/// <param name="rootShadingStep">The root shading step.</param>
/// <param name="mapProjection">The map projection to use.</param>
/// <returns>
/// A ShadedRasterImageGenerator function.
/// </returns>
let generateShadedRasterTile
    tileSize
    (dataSourcesFetchers: ShadingDataSourcesFetcher[])
    (createShaderFunction: ShadingFuncFactory)
    : ShadedRasterImageGenerator =
    fun demLevel (imageRect: Rect) rootShadingStep mapProjection ->

        let buffer = 1

        let x1 = float (imageRect.MinX - buffer)
        let y1 = float (imageRect.MinY - buffer)
        let lon1Rad, lat1Rad = mapProjection.Invert x1 y1 |> Option.get

        let x2 = float (imageRect.MaxX + buffer)
        let y2 = float (imageRect.MaxY + buffer)
        let lon2Rad, lat2Rad = mapProjection.Invert x2 y2 |> Option.get

        let lonLatBounds: LonLatBounds =
            { MinLon = radToDeg (min lon1Rad lon2Rad)
              MinLat = radToDeg (min lat1Rad lat2Rad)
              MaxLon = radToDeg (max lon1Rad lon2Rad)
              MaxLat = radToDeg (max lat1Rad lat2Rad) }

        let dataSourcesResult =
            dataSourcesFetchers
            |> Array.fold
                (fun dataSourcesResult fetcher ->
                    dataSourcesResult
                    |> Result.bind (fun dataSources ->
                        dataSources |> fetcher demLevel lonLatBounds))
                (Ok(ShadingDataSources.Create()))

        match dataSourcesResult with
        | Error errorMessage -> Error errorMessage
        | Ok dataSources ->
            let imageData =
                executeShadingStep
                    createShaderFunction
                    createCompositingFuncById
                    dataSources
                    tileSize
                    demLevel
                    imageRect
                    mapProjection.Proj
                    mapProjection.Invert
                    rootShadingStep

            Ok(Some imageData)

type ShadedRasterTileSaver =
    Options
        -> int
        -> int * int
        -> Rect
        -> RawImageData
        -> Result<string, string>

let saveShadedRasterTile
    (ensureDirectoryExists: DirectoryExistsEnsurer)
    (openFileToWrite: FileWriter)
    (writePngToStream: Png.File.PngStreamWriter)
    : ShadedRasterTileSaver =
    fun
        (options: Options)
        (maxTileIndex: int)
        (tileIndexX, tileIndexY)
        (tileRect: Rect)
        imageData ->

        ensureDirectoryExists options.OutputDir |> ignore

        let tileIndexStringWidth =
            int (ceil (Math.Log10(float (maxTileIndex + 1))))

        let tilePngFileName =
            options.OutputDir
            |> Pth.combine (
                sprintf
                    "%s-%0*d-%0*d.png"
                    options.FilePrefix
                    tileIndexStringWidth
                    tileIndexX
                    tileIndexStringWidth
                    tileIndexY
            )

        openFileToWrite tilePngFileName
        |> Result.map (fun stream ->
            let ihdr =
                { Width = tileRect.Width
                  Height = tileRect.Height
                  BitDepth = PngBitDepth.BitDepth8
                  ColorType = PngColorType.RgbAlpha
                  InterlaceMethod = PngInterlaceMethod.NoInterlace }

            stream |> writePngToStream ihdr imageData |> closeStream

            Log.info $"Saved a shade tile to %s{tilePngFileName}"

            tilePngFileName)
        |> Result.mapError fileSysErrorMessage




let runWithProjection
    tileSize
    mapProjection
    (options: Options)
    (generateImageTile: ShadedRasterImageGenerator)
    (saveImageTile: ShadedRasterTileSaver)
    : Result<unit, string> =
    let rasterMbr = calculateRasterMbr mapProjection options

    // calculate DEM level needed
    let demLevel = calculateDemLevelNeeded tileSize mapProjection rasterMbr

    // then split it up into tiles
    let tilesToGenerate, maxTileIndex =
        constructRasterTilesList options rasterMbr

    let generateAndSaveHillshadingTile
        xIndex
        yIndex
        imageRect
        tilesGeneratedSoFar
        =
        Log.info $"Generating a shade tile %d{xIndex}/%d{yIndex}..."

        generateImageTile
            demLevel
            imageRect
            options.RootShadingStep
            mapProjection
        |> Result.map (fun maybeGeneratedTile ->
            match maybeGeneratedTile with
            | Some imageData ->
                Log.info "Saving the shade tile..."

                saveImageTile
                    options
                    maxTileIndex
                    (xIndex, yIndex)
                    imageRect
                    imageData
                |> ignore

                tilesGeneratedSoFar + 1
            | None -> tilesGeneratedSoFar)


    tilesToGenerate
    |> List.fold
        (fun state (xIndex, yIndex, rasterTileRect) ->
            state
            |> Result.bind (
                generateAndSaveHillshadingTile xIndex yIndex rasterTileRect
            ))
        (Ok 0)
    |> Result.map (fun actualTilesGenerated ->
        match actualTilesGenerated with
        | 0 ->
            Log.info
                $"Nothing was generated since there were no SRTM tiles to work with. Are you sure '%s{options.SrtmDir}' directory contains SRTM tiles?"
        | _ -> ())

/// <summary>
/// The main function for generating shaded raster tiles.
/// </summary>
/// <remarks>
/// <para>
/// This function is the main entry point for the shading process. It takes in
/// the options for the shading process, a function to generate individual
/// shaded raster tiles, and a function to save these tiles as image files.
/// </para>
/// <para>
/// The function first creates a map projection using the projection and scale
/// specified in the options. It then calls the `runWithProjection` function,
/// passing in the created map projection along with the other parameters.
/// The `runWithProjection` function is responsible for generating and saving
/// the shaded raster tiles. It projects the coverage points, calculates the
/// minimum bounding rectangle of these points, determines the SRTM level
/// needed, and splits the area into tiles. For each tile, it generates the
/// shaded raster tile and saves it as an image file.
/// </para>
/// <para>
/// If all tiles are successfully generated and saved, the function returns a
/// result type containing unit. If any part of the process fails, it returns a
/// result type containing an error message.
/// </para>
/// </remarks>
/// <param name="options">
/// The options for the shading process, including parameters
/// like coverage points, root shading step, map scale, and map projection.
/// </param>
/// <param name="generateImageTile">
/// A function that generates an individual shaded raster tile.</param>
/// <param name="saveImageTile">
/// A function that saves a shaded raster tile as an image file.
/// </param>
/// <returns>
/// A result typ with ean error message if any part of the process fails
/// .</returns>
let run
    (options: Options)
    (generateImageTile: ShadedRasterImageGenerator)
    (saveImageTile: ShadedRasterTileSaver)
    : Result<unit, string> =

    createMapProjection options.MapProjection.Projection options.MapScale
    |> Result.bind (fun mapProjection ->
        runWithProjection
            Funcs.SrtmTileSize
            mapProjection
            options
            generateImageTile
            saveImageTile)
