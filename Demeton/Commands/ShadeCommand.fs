module Demeton.Commands.ShadeCommand

open Demeton.GeometryTypes
open Demeton.CommandLineParsing

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
    let splits = value.Split (',') 

    match splits.Length with
    | l when l % 2 <> 0 -> Error "has an odd number of coordinates"
    | l when l < 4 -> Error "has to have at least two points specified"
    | _ -> Ok context

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
    | Ok (_, finalOptions) ->
        match finalOptions.CoveragePoints |> Seq.length with
        | len when len < 2 -> 
            Error 
                (sprintf 
                    "'%s' parameter has to have at least two points specified." 
                    CoveragePointsParameter)
        | _ -> parsingResult
    | _ -> parsingResult
