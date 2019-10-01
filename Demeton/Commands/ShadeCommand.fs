module Demeton.Commands.ShadeCommand

open Demeton.Geometry
open Demeton.CommandLineParsing
open Demeton.Commands.ParametersParsing

type ShadeOptions = {
   CoveragePoints: LonLat seq
   MapScale: float
   Dpi: float
}

[<Literal>]
let CoveragePointsParameter = "coverage"
[<Literal>]
let MapScaleParameter = "map-scale"
[<Literal>]
let DpiParameter = "dpi"


let parseCoverage (value: string) (context: ParsingContext<ShadeOptions>) =
    let floatsListResult = parseFloatsList value

    match floatsListResult with
    | Error _ -> invalidOp "todo"
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


let parseShadeArgs (args: string list): ParsingResult<ShadeOptions> =
    let defaultOptions = { 
        CoveragePoints = []; MapScale = 50000.; Dpi = 300. }

    let mutable parsingResult: ParsingResult<ShadeOptions> = 
        Ok (args, defaultOptions)

    while hasMoreArgs parsingResult do
        let (arg, context) = nextArgResult parsingResult

        parsingResult <-
            match arg with
            | Some "--coverage" ->
                parseParameterValue CoveragePointsParameter parseCoverage context
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
