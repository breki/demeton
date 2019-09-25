module Demeton.Commands.ImportSrtmTilesCommand

open Demeton.GeometryTypes    
open Demeton.Srtm.Types
open Demeton.DemTypes
open Demeton.CommandLineParsing
open System
open System.Globalization
open System.IO
open Demeton.Srtm


type ImportOptions = {
    Bounds: Bounds option
    SrtmDir: string
    LocalCacheDir: string
}


let boundsParameter = "bounds"
let srtmDirParameter = "srtm-dir"
let localCacheDirParameter = "local-cache-dir"


let parseBounds (value: string) (context: ParsingContext<ImportOptions>) =
    let tryParseFloat (value: string) =
        match Double.TryParse
            (
            value,
            NumberStyles.Float,
            CultureInfo.InvariantCulture) with
        | (true, parsed) -> Some parsed
        | _ -> None

    let isLongitudeInRange value = value >= -179. && value <= 180.
    let isLatitudeInRange value = value >= -90. && value <= 90.

    let boundsFromParsedParts (parts: float option array)
        : Result<Bounds, string> =
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
            |> invalidParameter boundsParameter "it should consist of numbers only"
        | false ->
            let bounds = boundsFromParsedParts parsedSplits
            match bounds with
            | Error reason -> 
                context |> invalidParameter boundsParameter reason
            | Ok boundsVal -> 
                let (_, oldOptions) = context
                context 
                |> consumeArg
                |> withOptions ({ oldOptions with Bounds = Some boundsVal })
                |> Result.Ok

    | _ -> 
        context 
        |> invalidParameter boundsParameter "it should consist of 4 numbers"


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


let parseImportArgs (args: string list): ParsingResult<ImportOptions> =
    let defaultOptions = { 
        Bounds = None; SrtmDir = "srtm"; LocalCacheDir = "cache" }

    let mutable parsingResult: ParsingResult<ImportOptions> = 
        Ok (args, defaultOptions)

    while hasMoreArgs parsingResult do
        let (arg, context) = nextArgResult parsingResult

        parsingResult <-
            match arg with
            | Some "--bounds" ->
                parseParameterValue boundsParameter parseBounds context
            | Some "--srtm-dir" -> 
                parseParameterValue srtmDirParameter parseSrtmDir context
            | Some "--local-cache-dir" -> 
                parseParameterValue 
                    localCacheDirParameter parseLocalCacheDir context
            | Some unknownArg ->
                Error (sprintf "Unrecognized parameter '%s'." unknownArg)
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
let import 
    (tiles: SrtmTileCoords[])
    (readTile: SrtmTileReader)
    : unit = 

    tiles |> Array.Parallel.iter (fun tileCoords ->
        printf "Looking for SRTM tile %s... " (Tile.tileId tileCoords)
        let heightsArrayOption = readTile tileCoords
        match heightsArrayOption with
        | None -> printfn " the tile does not exist, moving to the next one."
        | _ -> printfn " imported."
        )

    ignore()