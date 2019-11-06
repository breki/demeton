[<RequireQualifiedAccess>]
module Demeton.Commands.ShadeCommand

open CommandLine
open CommandLine.Common
open Raster
open Demeton.Geometry
open Demeton.Geometry.Common
open Demeton.Projections
open Demeton.Shaders
open Demeton.Shaders.Types
open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.Pipeline.Parsing
open Demeton.Shaders.Pipeline.Building
open Demeton.Shaders.Pipeline.BuildingElevationColoring
open Demeton.Shaders.Pipeline.BuildingIgorHillshading
open Demeton.Srtm
open Demeton.Srtm.Funcs
open Png.Types
open Text

open System

type Options = {
    CoveragePoints: LonLat list
    FilePrefix: string
    LocalCacheDir: string
    OutputDir: string
    SrtmDir: string
    TileSize: int
    RootShadingStep: Pipeline.Common.ShadingStep 
    ShaderOptions: ShaderOptions
}

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

let registeredStepBuilders = dict [
    ("elecolor", elevationColoringStepBuilder)
    ("igor", igorHillshadingStepBuilder)
]

let parseShadingScriptOption: OptionValueParser = fun value ->
    match parseShadingScript value with
    | Ok parsedScript ->
        match buildShadingPipeline registeredStepBuilders parsedScript with
        | Ok rootStep -> OkValue rootStep
        | Error errorMessage -> 
            buildString()
            |> newLine
            |> append errorMessage
            |> toString
            |> InvalidValue 
    | Error parsingError -> 
        buildString()
        |> newLine
        |> appendLine value
        |> appendLine (sprintf "%s^" (new String(' ', parsingError.Location)))
        |> append parsingError.Message
        |> append "."
        |> toString
        |> InvalidValue 

let supportedParameters: CommandParameter[] = [|
    Arg.build CoveragePointsParameter
    |> Arg.desc "A list of points to be covered. At least two points need to be specified."
    |> Arg.format "x1,y1,x2,y2..."
    |> Arg.example "5,43.3,16.6,48.4" "fetches (roughly) the whole Alps area"
    |> Arg.parser parseCoverage |> Arg.toPar

    Option.build DpiParameter 
    |> Option.desc "The printing resolution required for the resulting raster image."
    |> Option.asPositiveFloat |> Option.defaultValue DefaultDpi
    |> Option.example "1200"  "specifies the printing resolution of 1200 dots per inch"
    |> Option.toPar

    Option.build FilePrefixParameter
    |> Option.desc "The text used to prefix names of all generated image files."
    |> Option.asFileName |> Option.defaultValue DefaultFilePrefix
    |> Option.example "hillshade"  "all generated image file names will start with 'hillshade', like 'hillshade-2-3.png"
    |> Option.toPar

    Option.build LocalCacheDirParameter
    |> Option.desc "The path to the local SRTM cache directory. The directory will be created if it does not exist yet."
    |> Option.asDirectory |> Option.defaultValue DefaultLocalCacheDir
    |> Option.toPar

    Option.build MapScaleParameter
    |> Option.desc "The map scale needed for the resulting raster image."
    |> Option.asFloat 1. |> Option.defaultValue DefaultMapScale
    |> Option.example "100000"  "the map scale of resulting raster image will be 1 : 100,000"
    |> Option.toPar

    Option.build OutputDirParameter
    |> Option.desc "The path to the directory where the raster files will be generated. The directory will be created if it does not exist yet."
    |> Option.asDirectory |> Option.defaultValue DefaultOutputDir
    |> Option.toPar

    Option.build ShadingScriptParameter 
    |> Option.desc "The path to the directory where the raster files will be generated. The directory will be created if it does not exist yet."
    |> Option.parser parseShadingScriptOption
    |> Option.defaultValue "elevation coloring + hillshading"
    // todo: add example
    //|> Option.example "1200"  "specifies the printing resolution of 1200 dots per inch"
    |> Option.toPar

    Option.build SrtmDirParameter
    |> Option.desc "The path to the directory containing the original zipped SRTM HGT files."
    |> Option.asDirectory |> Option.defaultValue DefaultSrtmDir
    |> Option.toPar

    Option.build TileSizeParameter 
    |> Option.desc 
        ("The maximum width and height in pixels of an individual raster image tile. "
        + "If the image is larger than this size, it will be split into multiple tiles.")
    |> Option.asPositiveInt |> Option.defaultValue DefaultTileSize
    |> Option.example "1200"  "specifies the printing resolution of 1200 dots per inch"
    |> Option.toPar
|]


let fillOptions parsedParameters =
    let igorShaderParameters: IgorHillshader.ShaderParameters = { 
        SunAzimuth = degToRad -90.
        ShadingColor = 0u }

    let shadingPipeline = 
        Pipeline.Common.Compositing
            (
                Pipeline.Common.ElevationColoring
                    { ColorScale = ElevationColoring.colorScaleMaperitive } ,
                Pipeline.Common.IgorHillshading igorShaderParameters,
                Demeton.Shaders.Pipeline.Common.CompositingFuncIdOver
            )

    let defaultOptions = 
        { 
            CoveragePoints = []
            FilePrefix = DefaultFilePrefix
            LocalCacheDir = DefaultLocalCacheDir
            OutputDir = DefaultOutputDir
            SrtmDir = DefaultSrtmDir
            TileSize = DefaultTileSize
            RootShadingStep = shadingPipeline
            ShaderOptions = { MapScale = DefaultMapScale; Dpi = DefaultDpi }
        }

    let processParameter options parameter =
        match parameter with
        | ParsedArg { Name = CoveragePointsParameter; Value = value } -> 
            { options with CoveragePoints = value :?> LonLat list }
        | ParsedOption { Name = DpiParameter; Value = value } ->
            { options with 
                ShaderOptions = 
                    { options.ShaderOptions with Dpi = value :?> float } }
        | ParsedOption { Name = FilePrefixParameter; Value = value } ->
            { options with FilePrefix = value :?> string }
        | ParsedOption { Name = LocalCacheDirParameter; Value = value } ->
            { options with LocalCacheDir = value :?> string }
        | ParsedOption { Name = MapScaleParameter; Value = value } ->
            { options with 
                ShaderOptions = 
                    { options.ShaderOptions with MapScale = value :?> float }}
        | ParsedOption { Name = OutputDirParameter; Value = value } ->
            { options with OutputDir = value :?> string }
        | ParsedOption { Name = ShadingScriptParameter; Value = value } ->
            { options with RootShadingStep = value :?> ShadingStep }
        | ParsedOption { Name = SrtmDirParameter; Value = value } ->
            { options with SrtmDir = value :?> string }
        | ParsedOption { Name = TileSizeParameter; Value = value } ->
            { options with TileSize = value :?> int }
        | _ -> invalidOp "Unrecognized parameter."

    parsedParameters 
    |> List.fold processParameter defaultOptions


let splitIntoIntervals minValue maxValue intervalSize =
    let spaceLength = maxValue - minValue
    let intervalsRemainder = 
        match spaceLength % intervalSize with
        | 0 -> 0
        | _ -> 1

    let intervalsCount = spaceLength / intervalSize + intervalsRemainder

    Seq.init 
        intervalsCount 
        (fun intervalIndex -> 
            let intervalMinValue = minValue + intervalIndex * intervalSize
            let intervalMaxValue = 
                min (intervalMinValue + intervalSize) maxValue
            (intervalIndex, intervalMinValue, intervalMaxValue)
        )

type ShadedRasterTileGenerator = 
    Raster.Rect -> Options -> Result<RawImageData option, string>

let generateShadedRasterTile 
    (fetchHeightsArray: SrtmHeightsArrayFetcher)
    (createShaderFunction: Demeton.Shaders.Pipeline.Common.ShadingFuncFactory)
    : ShadedRasterTileGenerator = 
    fun (tileRect: Raster.Rect) options ->

    let scaleFactor = options.ShaderOptions.ProjectionScaleFactor

    let buffer = 1

    let x1 = float (tileRect.MinX - buffer) / scaleFactor
    let y1 = float (tileRect.MinY - buffer) / scaleFactor
    let (lon1Rad, lat1Rad) = WebMercator.inverse x1 -y1 |> Option.get

    let x2 = float (tileRect.MaxX + buffer) / scaleFactor
    let y2 = float (tileRect.MaxY + buffer) / scaleFactor
    let (lon2Rad, lat2Rad) = WebMercator.inverse x2 -y2 |> Option.get

    let lonLatBounds: LonLatBounds = 
        { 
            MinLon = radToDeg (min lon1Rad lon2Rad)
            MinLat = radToDeg (min lat1Rad lat2Rad)
            MaxLon = radToDeg (max lon1Rad lon2Rad)
            MaxLat = radToDeg (max lat1Rad lat2Rad)
        }

    let srtmTilesNeeded = boundsToTiles lonLatBounds

    let heightsArrayResult = fetchHeightsArray srtmTilesNeeded

    match heightsArrayResult with
    | Error errorMessage -> Error errorMessage
    | Ok heightArrayOption ->
        match heightArrayOption with
        | Some heightsArray ->
            let imageData = 
                Pipeline.Common.executeShadingStep 
                    createShaderFunction
                    Demeton.Shaders.Pipeline.Common.createCompositingFuncById
                    heightsArray 
                    tileRect 
                    options.ShaderOptions 
                    options.RootShadingStep 
            Ok (Some imageData)
        | None -> Ok None
    
type ShadedRasterTileSaver = 
    Options -> int -> (int * int) -> Raster.Rect -> RawImageData -> string

let saveShadedRasterTile 
    (ensureDirectoryExists: FileSys.DirectoryExistsEnsurer)
    (openFileToWrite: FileSys.FileOpener)
    (writePngToStream: Png.File.PngStreamWriter)
    (options: Options) 
    (maxTileIndex: int)
    (tileIndexX, tileIndexY)
    (tileRect: Raster.Rect)
    imageData =

    ensureDirectoryExists options.OutputDir |> ignore

    let tileIndexStringWidth = 
        int (ceil (Math.Log10(float (maxTileIndex + 1))))

    let tilePngFileName =
        options.OutputDir 
        |> Pth.combine (
            sprintf "%s-%0*d-%0*d.png" 
                options.FilePrefix 
                tileIndexStringWidth 
                tileIndexX 
                tileIndexStringWidth 
                tileIndexY)
    use stream = openFileToWrite tilePngFileName

    let ihdr = {
        Width = tileRect.Width
        Height = tileRect.Height
        BitDepth = PngBitDepth.BitDepth8
        ColorType = PngColorType.RgbAlpha
        InterlaceMethod = PngInterlaceMethod.NoInterlace
    }

    stream |> writePngToStream ihdr imageData |> ignore
    
    Log.info "Saved a shade tile to %s" tilePngFileName

    tilePngFileName

let run 
    (options: Options) 
    (generateTile: ShadedRasterTileGenerator) 
    (saveTile: ShadedRasterTileSaver)
    : Result<string, string> list =
    // project each coverage point
    let projectedPoints = 
        options.CoveragePoints 
        |> List.map (
            fun (lonDegrees, latDegrees) -> 
                WebMercator.proj (degToRad lonDegrees) (degToRad latDegrees))
        |> List.filter (fun p -> Option.isSome p)
        |> List.map (fun p -> Option.get p)
        |> List.map (fun (x, y) -> (x, -y))

    // calculate the minimum bounding rectangle of all the projected points
    let projectionMbr = Bounds.mbrOf projectedPoints

    // calculate MBR in terms of pixels
    let scaleFactor = options.ShaderOptions.ProjectionScaleFactor

    let rasterMbr = projectionMbr |> Bounds.multiply scaleFactor

    // round off the raster so we work with integer coordinates
    let rasterMbrRounded = 
        Raster.Rect.asMinMax
            (int (floor rasterMbr.MinX))
            (int (floor rasterMbr.MinY))
            (int (ceil rasterMbr.MaxX))
            (int (ceil rasterMbr.MaxY))

    // then split it up into tiles
    let tileSize = options.TileSize

    let tilesToGenerate = 
        [
                for (yIndex, tileMinY, tileMaxY) in splitIntoIntervals 
                    rasterMbrRounded.MinY rasterMbrRounded.MaxY tileSize do
                    for (xIndex, tileMinX, tileMaxX) in splitIntoIntervals 
                        rasterMbrRounded.MinX rasterMbrRounded.MaxX tileSize do
                        let tileBounds = 
                            Raster.Rect.asMinMax 
                                tileMinX tileMinY tileMaxX tileMaxY

                        yield (xIndex, yIndex, tileBounds)
        ]

    let maxTileIndex = 
        tilesToGenerate 
        |> List.map (fun (xIndex, yIndex, _) -> max xIndex yIndex)
        |> List.max

    tilesToGenerate 
    |> List.choose (fun (xIndex, yIndex, tileBounds) ->
        Log.info "Generating a shade tile %d/%d..." xIndex yIndex

        let tileGenerationResult = generateTile tileBounds options

        match tileGenerationResult with
        | Error errorMessage -> Some (Error errorMessage)
        | Ok maybeGeneratedTile ->
            match maybeGeneratedTile with
            | Some imageData -> 
                let tileImageFileName = 
                    saveTile 
                        options 
                        maxTileIndex 
                        (xIndex, yIndex) 
                        tileBounds 
                        imageData
                Some (Ok tileImageFileName)
            | None  -> None
        )
