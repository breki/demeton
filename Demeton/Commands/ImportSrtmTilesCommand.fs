[<RequireQualifiedAccess>]
module Demeton.Commands.ImportSrtmTilesCommand

open Demeton.CommandLineParsing
open Demeton.DemTypes
open Demeton.Geometry.Common
open Demeton.Srtm
open Demeton.Srtm.Types

open System
open System.IO


type Options = {
    Bounds: LonLatBounds option
    SrtmDir: string
    LocalCacheDir: string
}

[<Literal>]
let BoundsParameter = "bounds"
[<Literal>]
let SrtmDirParameter = "srtm-dir"
[<Literal>]
let LocalCacheDirParameter = "local-cache-dir"


let parseBounds (value: string) (context: ParsingContext<Options>) =

    let isLongitudeInRange value = value >= -179. && value <= 180.
    let isLatitudeInRange value = value >= -90. && value <= 90.

    let boundsFromParsedParts (parts: float option array)
        : Result<LonLatBounds, string> =
        let minLon = Option.get parts.[0]
        let minLat = Option.get parts.[1]
        let maxLon = Option.get parts.[2]
        let maxLat = Option.get parts.[3]
        match (minLon, minLat, maxLon, maxLat) with
        | (x, _, _, _) when not (isLongitudeInRange x) 
            -> Error "longitude value is out of range"
        | (_, _, x, _) when not (isLongitudeInRange x) 
            -> Error "longitude value is out of range"
        | (_, x, _, _) when not (isLatitudeInRange x) 
            -> Error "latitude value is out of range"
        | (_, _, _, x) when not (isLatitudeInRange x) 
            -> Error "latitude value is out of range"
        | (min, _, max, _) when min > max 
            -> Error "max longitude value is smaller than min longitude value"
        | (_, min, _, max) when min > max 
            -> Error "max latitude value is smaller than min latitude value"
        | _ -> Ok { 
                    MinLon = minLon
                    MinLat = minLat
                    MaxLon = maxLon
                    MaxLat = maxLat
                }

    let splits = value.Split (',') 

    match splits.Length with
    | 4 -> 
        let parsedSplits = splits |> Array.map tryParseFloat
        let hasAnyInvalidParts = parsedSplits |> Array.exists Option.isNone
        match hasAnyInvalidParts with
        | true -> 
            context 
            |> invalidParameter 
                BoundsParameter "it should consist of 4 comma-separated numbers"
        | false ->
            let bounds = boundsFromParsedParts parsedSplits
            match bounds with
            | Error reason -> 
                context |> invalidParameter BoundsParameter reason
            | Ok boundsVal -> 
                let (_, oldOptions) = context
                context 
                |> consumeArg
                |> withOptions ({ oldOptions with Bounds = Some boundsVal })
                |> Result.Ok

    | _ -> 
        context 
        |> invalidParameter BoundsParameter "it should consist of 4 numbers"


let parseSrtmDir value context =
    let (_, oldOptions) = context
    context 
    |> consumeArg
    |> withOptions ({ oldOptions with SrtmDir = value })
    |> Result.Ok


let parseLocalCacheDir value context =
    let (_, oldOptions) = context
    context 
    |> consumeArg
    |> withOptions ({ oldOptions with LocalCacheDir = value })
    |> Result.Ok


let parseArgs (args: string list): ParsingResult<Options> =
    let defaultOptions = { 
        Bounds = None; SrtmDir = "srtm"; LocalCacheDir = "cache" }

    let supportedParameters = [|
        { Name = BoundsParameter; 
            Parser = parseParameterValue parseBounds }
        { Name = SrtmDirParameter; 
            Parser = parseParameterValue parseSrtmDir }
        { Name = LocalCacheDirParameter; 
            Parser = parseParameterValue parseLocalCacheDir }
    |]

    let mutable parsingResult: ParsingResult<Options> = 
        Ok (args, defaultOptions)

    while hasMoreArgs parsingResult do
        let (arg, context) = nextArgResult parsingResult

        parsingResult <-
            match arg with
            | Some argParameter when argParameter.StartsWith("--") ->
                let parameterName = argParameter.Substring 2
                let parameterMaybe =
                    supportedParameters 
                    |> Array.tryFind (fun p -> 
                        String.Equals(
                            parameterName, 
                            p.Name, 
                            StringComparison.OrdinalIgnoreCase))
                match parameterMaybe with
                | Some parInfo -> parInfo.Parser parameterName context
                | None -> 
                    Error (sprintf "Unrecognized parameter '%s'." parameterName)
            | Some argParameter -> 
                Error (sprintf "Unrecognized parameter '%s'." argParameter)
            | None -> invalidOp "BUG: this should never happen"

    match parsingResult with
    | Ok (_, finalOptions) ->
        match finalOptions.Bounds with
        | None -> Error "'bounds' parameter is missing."
        | _ -> parsingResult
    | _ -> parsingResult


type SrtmToPngEncoder = HeightsArray -> Stream -> unit


/// <summary>
/// A generic method for importing the specified SRTM tiles into the local
/// storage, encoded as PNG files.
/// </summary>
/// <param name="tiles">A sequence of SRTM tiles to import.</param>
/// <param name="readTile">
/// A function that transforms a SRTM tile into <see cref="HeightsArray" />.
/// This typically means the function should download the zipped HGT file,
/// unzip it and read it as <see cref="HeightsArray" /> instance.
/// </param>
/// <param name="createPngTileFile">
/// A function that creates a new PNG file for the given SRTM tile ID and
/// provides a writable stream to it.
/// </param>
/// <param name="encodeSrtmTileToPng">
/// A function that encodes the <see cref="HeightsArray" /> as a PNG image and
/// writes it into the provides stream.
/// </param>
let run 
    (tiles: SrtmTileCoords[])
    (checkCaching: Tile.CachingStatusChecker)
    (readTile: SrtmTileReader)
    : unit = 

    tiles |> Array.Parallel.iter (fun tileCoords ->
        let cachingStatus = checkCaching tileCoords

        match cachingStatus with
        | Tile.CachingStatus.NotCached ->
            let tileId = (Tile.tileId tileCoords)
            Log.info "Importing SRTM tile %s... " tileId
            let heightsArrayOption = readTile tileCoords
            match heightsArrayOption with
            | Ok None -> 
                Log.info 
                    "Tile %s does not exist, moving to the next one." tileId
            | Ok _ -> Log.info "Tile %s imported." tileId
            | Error msg -> 
                Log.error "Tile %s could not be imported: %s." tileId msg
        | _ -> ignore()

        )

    Log.info "All tiles were imported into the local cache."

    ignore()