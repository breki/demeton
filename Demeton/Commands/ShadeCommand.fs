[<RequireQualifiedAccess>]
module Demeton.Commands.ShadeCommand

open Demeton
open Demeton.CommandLineParsing
open Demeton.Commands.ParametersParsing
open Demeton.DemTypes
open Demeton.Geometry
open Demeton.Geometry.Common
open Demeton.Projections
open Demeton.Projections.Common
open Demeton.Srtm
open Demeton.Srtm.Funcs
open Png
open Png.Types

open System.IO
open Demeton.Srtm
open System

type Options = {
    CoveragePoints: LonLat list
    Dpi: float
    FileName: string
    MapScale: float
    OutputDir: string
}

[<Literal>]
let CoveragePointsParameter = "coverage"
[<Literal>]
let MapScaleParameter = "map-scale"
[<Literal>]
let DpiParameter = "dpi"
[<Literal>]
let OutputDirParameter = "output-dir"
[<Literal>]
let FileNameParameter = "file-name"


let parseCoverage (value: string) (context: ParsingContext<Options>) =
    let floatsListResult = parseFloatsList value

    match floatsListResult with
    | Error _ -> 
        context |> invalidParameter 
            CoveragePointsParameter "it has to consist of a list of coordinates"
    | Ok floatsList ->
        match floatsList.Length with
        | l when l % 2 <> 0 -> 
            context |> invalidParameter 
                CoveragePointsParameter "it has an odd number of coordinates"
        | l when l < 4 -> 
            context |> invalidParameter 
                CoveragePointsParameter 
                    "it has to have at least two points specified"
        | _ -> 
            let (_, oldOptions) = context
            context 
            |> consumeArg
            |> withOptions 
                ({ oldOptions 
                    with CoveragePoints = floatsListToPoints floatsList })
            |> Result.Ok


let parseMapScale (value: string) (context: ParsingContext<Options>) =
    let floatResult = parseFloat value

    match floatResult with
    | Error _ -> 
        context |> invalidParameter 
            MapScaleParameter  "it has to be a numeric value larger than 1"
    | Ok value ->
        match value with
        | x when x < 1. -> 
            context |> invalidParameter 
                MapScaleParameter  "it has to be a value larger than 1"
        | _ -> 
            let (_, oldOptions) = context
            context 
            |> consumeArg
            |> withOptions ({ oldOptions with MapScale = value })
            |> Result.Ok

let parseDpi (value: string) (context: ParsingContext<Options>) =
    let floatResult = parseFloat value

    match floatResult with
    | Error _ -> 
        context |> invalidParameter 
            DpiParameter  "it has to be a positive numeric value"
    | Ok value ->
        match value with
        | x when x <= 0. -> 
            context |> invalidParameter 
                DpiParameter "it has to be a positive numeric value"
        | _ -> 
            let (_, oldOptions) = context
            context 
            |> consumeArg
            |> withOptions ({ oldOptions with Dpi = value })
            |> Result.Ok

let parseFileName (value: string) (context: ParsingContext<Options>) =
    let checkedValue = Path.GetFileName(value)

    match checkedValue = value with
    | false -> 
        context |> invalidParameter 
            FileNameParameter "it has to consist of valid path characters"
    | true ->
        let (_, oldOptions) = context
        context 
        |> consumeArg
        |> withOptions ({ oldOptions with FileName = value })
        |> Result.Ok

let parseOutputDir (value: string) (context: ParsingContext<Options>) =
    let (_, oldOptions) = context
    context 
    |> consumeArg
    |> withOptions ({ oldOptions with OutputDir = value })
    |> Result.Ok

let parseArgs (args: string list): ParsingResult<Options> =
    let defaultOptions = 
        { 
            CoveragePoints = []
            Dpi = 300. 
            FileName = "shading"
            MapScale = 50000. 
            OutputDir = "output"
        }

    let mutable parsingResult: ParsingResult<Options> = 
        Ok (args, defaultOptions)

    while hasMoreArgs parsingResult do
        let (arg, context) = nextArgResult parsingResult

        parsingResult <-
            match arg with
            | Some "--coverage" ->
                parseParameterValue 
                    CoveragePointsParameter parseCoverage context
            | Some "--dpi" ->
                parseParameterValue DpiParameter parseDpi context
            | Some "--file-name" ->
                parseParameterValue FileNameParameter parseFileName context
            | Some "--map-scale" ->
                parseParameterValue MapScaleParameter parseMapScale context
            | Some "--output-dir" ->
                parseParameterValue OutputDirParameter parseOutputDir context
            | Some unknownArg ->
                Error (sprintf "Unrecognized parameter '%s'." unknownArg)
            | None -> invalidOp "BUG: this should never happen"

    match parsingResult with
    | Ok context ->
        let (_, finalOptions) = context

        match finalOptions.CoveragePoints |> Seq.length with
        | len when len < 2 ->
            context
            |> invalidParameter 
                    CoveragePointsParameter
                    "it has to have at least two points specified" 
        | _ -> parsingResult
    | _ -> parsingResult

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

let shadeRaster: RasterShader = 
    fun heightsArray tileRect imageData options ->

    let tileWidth = tileRect.Width
    let scaleFactor = options |> projectionScaleFactor

    let heightForTilePixel x y =
        let xUnscaled = float x / scaleFactor
        let yUnscaled = float y / scaleFactor
        let lonLatOption = WebMercator.inverse xUnscaled yUnscaled

        match lonLatOption with
        | None -> None
        | Some (lon, lat) ->
            let globalSrtmX = Tile.longitudeToGlobalX lon 3600
            let globalSrtmY = Tile.latitudeToGlobalY lat 3600
            heightsArray.interpolateHeightAt (globalSrtmX, globalSrtmY)

    for y in tileRect.MinY .. (tileRect.MaxY-1) do
        for x in tileRect.MinX .. (tileRect.MaxX-1) do
            let height = heightForTilePixel x y

            let pixelValue = 
                match height with
                | None -> Rgba8Bit.rgbaColor 0uy 0uy 0uy 0uy
                | Some heightValue ->
                    let heightToByte = byte(min (heightValue / 10.0) 255.0)

                    Rgba8Bit.rgbaColor 
                        heightToByte heightToByte heightToByte 255uy

            Rgba8Bit.setPixelAt 
                imageData
                tileWidth
                (x - tileRect.MinX) 
                (y - tileRect.MinY)
                pixelValue


type ShadedRasterTileGenerator = 
    Raster.Rect -> Options -> Result<RawImageData option, string>

let generateShadedRasterTile 
    (tileRect: Raster.Rect)
    options 
    (fetchHeightsArray: SrtmHeightsArrayFetcher)
    (shadeRaster: RasterShader)
    : Result<unit option, string> =

    let scaleFactor = options |> projectionScaleFactor

    let x1 = float tileRect.MinX / scaleFactor
    let y1 = float tileRect.MinY / scaleFactor
    let (lon1, lat1) = WebMercator.inverse x1 y1 |> Option.get

    let x2 = float tileRect.MaxX / scaleFactor
    let y2 = float tileRect.MaxY / scaleFactor
    let (lon2, lat2) = WebMercator.inverse x2 y2 |> Option.get

    let lonLatBounds: LonLatBounds = 
        { 
            MinLon = min lon1 lon2
            MinLat = min lat1 lat2
            MaxLon = max lon1 lon2
            MaxLat = max lat1 lat2
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

            shadeRaster heightsArray tileRect imageData options
            Ok None
        | None -> Ok None
    
type ShadedRasterTileSaver = 
    Options -> int -> int -> Raster.Rect -> RawImageData -> string

let saveShadedRasterTile 
    ensureDirectoryExists
    (openFileToWrite: string -> Stream)
    (writePngToStream: Png.File.PngStreamWriter)
    (options: Options) 
    (maxTileIndex: int)
    tileIndexX 
    tileIndexY 
    (tileRect: Raster.Rect)
    imageData =

    ensureDirectoryExists options.OutputDir

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

    tilePngFileName

let run 
    (options: Options) 
    (generateTile: ShadedRasterTileGenerator) 
    (saveTile: ShadedRasterTileSaver) =
    // project each coverage point
    let projectedPoints = 
        options.CoveragePoints 
        |> List.map (fun (lon, lat) -> WebMercator.proj lon lat)
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

    // then split it up into 1000x1000 tiles
    let tileSize = 1000

    for (yIndex, tileMinY, tileMaxY) in splitIntoIntervals 
        rasterMbrRounded.MinY rasterMbrRounded.MaxY tileSize do
        for (xIndex, tileMinX, tileMaxX) in splitIntoIntervals 
            rasterMbrRounded.MinX rasterMbrRounded.MaxX tileSize do
            let tileBounds = 
                Raster.Rect.asMinMax tileMinX tileMinY tileMaxX tileMaxY
            
            let tileGenerationResult = generateTile tileBounds options

            match tileGenerationResult with
            | Error _ -> invalidOp "todo"
            | Ok maybeGeneratedTile ->
                match maybeGeneratedTile with
                | Some imageData -> 
                    saveTile options xIndex yIndex tileBounds imageData
                    |> ignore
                | None  -> invalidOp "todo"

            ignore()
