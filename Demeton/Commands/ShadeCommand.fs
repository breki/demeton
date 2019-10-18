[<RequireQualifiedAccess>]
module Demeton.Commands.ShadeCommand

open CommandLine
open CommandLine.Common
open Demeton
open Demeton.DemTypes
open Demeton.Geometry
open Demeton.Geometry.Common
open Demeton.Projections
open Demeton.Projections.Common
open Demeton.Shaders.ElevationColoring
open Demeton.Shaders.Hillshading
open Demeton.Shaders.Terrain
open Demeton.Shaders.ShaderTypes
open Demeton.Srtm
open Demeton.Srtm.Funcs
open Png
open Png.Types

open System

type Options = {
    CoveragePoints: LonLat list
    Dpi: float
    FileName: string
    LocalCacheDir: string
    MapScale: float
    OutputDir: string
    SrtmDir: string
    TileSize: int
    Shader: Shader
}

[<Literal>]
let CoveragePointsParameter = "coverage"
[<Literal>]
let DpiParameter = "dpi"
[<Literal>]
let ElevationColorShaderParameter = "elev-color"
[<Literal>]
let FileNameParameter = "file-name"
[<Literal>]
let LocalCacheDirParameter = "local-cache-dir"
[<Literal>]
let MapScaleParameter = "map-scale"
[<Literal>]
let OutputDirParameter = "output-dir"
[<Literal>]
let SrtmDirParameter = "srtm-dir"
[<Literal>]
let TileSizeParameter = "tile-size"


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


let parseElevationColorShader value =
    OkValue (ElevationColoringShader (elevationColorScaleMaperitive))


let supportedParameters: CommandParameter[] = [|
    Arg { 
        Name = CoveragePointsParameter; 
        Description = "A list of points to be covered. At least two points need to be specified."
        Format = "x1,y1,x2,y2..."
        Example = Some 
            ("5,43.3,16.6,48.4", "fetches (roughly) the whole Alps area")
        Parser = parseCoverage }
    Option { 
        Name = DpiParameter 
        Description = "The printing resolution required for the resulting raster image."
        ValuePlaceholder = "number"
        Format = "positive real number"
        Default = 300.
        Example = Some ("1200", "specifies the printing resolution of 1200 dots per inch")
        Parser = ValueParsers.parsePositiveFloat }
    Switch { Name = ElevationColorShaderParameter }
    Option { 
        Name = FileNameParameter
        Description = "The text used to prefix names of all generated image files."
        ValuePlaceholder = "text"
        Format = "text"
        Default = "shading"
        Example = Some ("hillshade", "all generated image file names will start with 'hillshade', like 'hillshade-2-3.png")
        Parser = ValueParsers.parseFileName }
    Option { 
        Name = LocalCacheDirParameter
        Description = "The path to the local SRTM cache directory. The directory will be created if it does not exist yet."
        ValuePlaceholder = "path"
        Format = "directory path"
        Default = "cache"
        Example = None
        Parser = ValueParsers.parseDir }
    Option { 
        Name = MapScaleParameter
        Description = "The map scale needed for the resulting raster image."
        ValuePlaceholder = "number"
        Format = "real number >= 1"
        Default = 50000.
        Example = Some("100000", "the map scale of resulting raster image will be 1 : 100,000")
        Parser = ValueParsers.parseFloat 1.}
    Option { 
        Name = OutputDirParameter
        Description = "The path to the directory where the raster files will be generated. The directory will be created if it does not exist yet."
        ValuePlaceholder = "path"
        Format = "directory path"
        Default = "option"
        Example = None
        Parser = ValueParsers.parseDir }
    Option { 
        Name = SrtmDirParameter
        Description = "The path to the directory containing the original zipped SRTM HGT files."
        ValuePlaceholder = "path"
        Format = "directory path"
        Default = "srtm"
        Example = None
        Parser = ValueParsers.parseDir }
    Option { 
        Name = TileSizeParameter
        Description = 
            "The maximum width and height in pixels of an individual raster image tile. "
            + "If the image is larger than this size, it will be split into multiple tiles."
        ValuePlaceholder = "number"
        Format = "positive integer value"
        Default = 1000
        Example = None
        Parser = ValueParsers.parsePositiveInt }
|]


let fillOptions parsedParameters =
    let defaultOptions = 
        { 
            CoveragePoints = []
            Dpi = 300. 
            FileName = "shading"
            LocalCacheDir = "cache"
            MapScale = 50000.             
            OutputDir = "output"
            SrtmDir = "srtm"
            TileSize = 1000
            Shader = ElevationColoringShader elevationColorScaleMaperitive
        }

    let processParameter options parameter =
        match parameter with
        | ParsedArg { Name = CoveragePointsParameter; Value = value } -> 
            { options with CoveragePoints = value :?> LonLat list }
        | ParsedOption { Name = DpiParameter; Value = value } ->
            { options with Dpi = value :?> float }
        | ParsedSwitch { Name = ElevationColorShaderParameter } ->
            { options with 
                Shader = ElevationColoringShader elevationColorScaleMaperitive }
        | ParsedOption { Name = FileNameParameter; Value = value } ->
            { options with FileName = value :?> string }
        | ParsedOption { Name = LocalCacheDirParameter; Value = value } ->
            { options with LocalCacheDir = value :?> string }
        | ParsedOption { Name = MapScaleParameter; Value = value } ->
            { options with MapScale = value :?> float }
        | ParsedOption { Name = OutputDirParameter; Value = value } ->
            { options with OutputDir = value :?> string }
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

let projectionScaleFactor options =
    EarthRadiusInMeters / options.MapScale * InchesPerMeter * options.Dpi

type RasterShader = 
    HeightsArray -> Raster.Rect -> RawImageData -> Options -> unit

/// <summary>
/// Fetches an appropriate <see cref="RasterShader" /> function based on the
/// specified shade command options.
/// </summary>
type RasterShaderFactory = Options -> RasterShader

let colorRasterBasedOnElevation: RasterShader = 
    fun heightsArray tileRect imageData options ->

    let tileWidth = tileRect.Width
    let scaleFactor = options |> projectionScaleFactor

    let heightForTilePixel x y =
        let xUnscaled = float x / scaleFactor
        let yUnscaled = float y / scaleFactor
        let lonLatOption = WebMercator.inverse xUnscaled yUnscaled

        match lonLatOption with
        | None -> None
        | Some (lonRad, latRad) ->
            let lonDeg = radToDeg lonRad
            let latDeg = radToDeg latRad

            let globalSrtmX = Tile.longitudeToGlobalX lonDeg 3600
            let globalSrtmY = Tile.latitudeToGlobalY latDeg 3600
            heightsArray.interpolateHeightAt (globalSrtmX, globalSrtmY)

    for y in tileRect.MinY .. (tileRect.MaxY-1) do
        for x in tileRect.MinX .. (tileRect.MaxX-1) do
            let height = heightForTilePixel x y

            let pixelValue = 
                elevationColorScaleMaperitive |> colorOfHeight height

            Rgba8Bit.setPixelAt 
                imageData
                tileWidth
                (x - tileRect.MinX) 
                (y - tileRect.MinY)
                pixelValue

let shadeRaster: RasterShader = 
    fun heightsArray tileRect imageData options ->

    let tileWidth = tileRect.Width
    let scaleFactor = options |> projectionScaleFactor

    let heightOf x y =
        let xUnscaled = x / scaleFactor
        let yUnscaled = y / scaleFactor
        let lonLatOption = WebMercator.inverse xUnscaled yUnscaled

        match lonLatOption with
        | None -> None
        | Some (lonRad, latRad) ->
            let lonDeg = radToDeg lonRad
            let latDeg = radToDeg latRad

            let globalSrtmX = Tile.longitudeToGlobalX lonDeg 3600
            let globalSrtmY = Tile.latitudeToGlobalY latDeg 3600
            heightsArray.interpolateHeightAt (globalSrtmX, globalSrtmY)

    let shaderParameters: ShaderParameters = {
            SunAzimuth = degToRad 45.
            ShadingIntensity = 1.
            ShadingColorR = 0uy
            ShadingColorG = 0uy
            ShadingColorB = 0uy
        }

    for y in tileRect.MinY .. (tileRect.MaxY-1) do
        for x in tileRect.MinX .. (tileRect.MaxX-1) do
            let pixelWidthInMeters = invalidOp "todo"
            let pixelHeightInMeters = invalidOp "todo"

            let cornerHeights = [|
                heightOf ((float x) - 0.5) ((float y) - 0.5)
                heightOf ((float x) + 0.5) ((float y) - 0.5)
                heightOf ((float x) - 0.5) ((float y) + 0.5)
                heightOf ((float x) + 0.5) ((float y) + 0.5)
                |]

            let slopeAndAspectMaybe =
                calculateSlopeAndAspect 
                    cornerHeights pixelWidthInMeters pixelHeightInMeters

            match slopeAndAspectMaybe with
            | Some (slope, aspect) ->
                let pixelValue = igorHillshade shaderParameters 0. slope aspect
                Rgba8Bit.setPixelAt 
                    imageData
                    tileWidth
                    (x - tileRect.MinX) 
                    (y - tileRect.MinY)
                    pixelValue
            | None -> ignore()

    invalidOp "todo"

let rasterShaderFactory: RasterShaderFactory = fun options ->
    // todo: extend the factory to support various shaders based on options
    colorRasterBasedOnElevation

type ShadedRasterTileGenerator = 
    Raster.Rect -> Options -> Result<RawImageData option, string>

let generateShadedRasterTile 
    (fetchHeightsArray: SrtmHeightsArrayFetcher)
    (createRasterShader: RasterShaderFactory)
    (tileRect: Raster.Rect)
    options 
    : Result<RawImageData option, string> =

    let scaleFactor = options |> projectionScaleFactor

    let x1 = float tileRect.MinX / scaleFactor
    let y1 = float tileRect.MinY / scaleFactor
    let (lon1Rad, lat1Rad) = WebMercator.inverse x1 y1 |> Option.get

    let x2 = float tileRect.MaxX / scaleFactor
    let y2 = float tileRect.MaxY / scaleFactor
    let (lon2Rad, lat2Rad) = WebMercator.inverse x2 y2 |> Option.get

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
                Rgba8Bit.createImageData 
                    tileRect.Width tileRect.Height Rgba8Bit.ImageDataZero

            let shadeRaster = createRasterShader options

            shadeRaster heightsArray tileRect imageData options
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
                options.FileName 
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
    
    Log.info "Saved a shade tile %s" tilePngFileName

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

    // calculate the minimum bounding rectangle of all the projected points
    let projectionMbr = Bounds.mbrOf projectedPoints

    // calculate MBR in terms of pixels
    let scaleFactor = options |> projectionScaleFactor

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
