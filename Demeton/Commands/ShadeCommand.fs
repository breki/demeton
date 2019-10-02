[<RequireQualifiedAccess>]
module Demeton.Commands.ShadeCommand

open Demeton
open Demeton.CommandLineParsing
open Demeton.Commands.ParametersParsing
open Demeton.Geometry.Common
open Demeton.Projections

open System.IO

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

type RasterTileGenerator = Raster.Rect -> Options -> unit

let run (options: Options) (rasterTileGenerator: RasterTileGenerator) =
    // project each coverage point
    let projectedPoints = 
        options.CoveragePoints 
        |> List.map (fun (lon, lat) -> WebMercator.proj lon lat)

    // calculate the minimum bounding rectangle of all the projected points

    // first calculate the total size of the raster

    // then split it up into 1000x1000 tiles

    invalidOp "todo"