module Demeton.Commands.ImportSrtmTilesCommand

open Demeton.GeometryTypes    
open Demeton.SrtmTypes
open Demeton.DemTypes
open System
open System.Globalization
open System.IO


type ImportOptions = {
    Bounds: Bounds option
    SrtmDir: string
    LocalCacheDir: string
}


type ParsingContext = string list * ImportOptions
type ParsingResult = Result<ParsingContext, string>


let nextArg (context: ParsingContext) =
    let (args, _) = context 
    match args.Length with
    | 0 -> None
    | _ -> Some (args |> List.head)


let nextArgResult (result: ParsingResult): (string option * ParsingContext) =
    match result with
    | Ok (args, options) -> 
        match args.Length with
        | 0 -> (None, ([], options))
        | _ -> (Some (args |> List.head), (args |> List.tail, options))
    | _ -> invalidOp "Parsing is already in a failed state."


let consumeArg (context: ParsingContext) =
    let (args, result) = context 
    (args |> List.tail, result)


let hasMoreArgs (context: ParsingContext) =
    let (args, _) = context
    args.Length > 0


let hasMoreArgsResult (context: ParsingResult) =
    match context with
    | Ok (args, _) -> args.Length > 0
    | Error _ -> false


let withError errorMessage (_: ParsingContext) =
    Error errorMessage


let withOptions (updatedOptions: ImportOptions) (context: ParsingContext)
    :ParsingContext =
    let (args, _) = context
    (args, updatedOptions)

let parseBounds (context: ParsingContext) =
    match nextArg context with
    | None -> context |> withError "`bounds` parameter's value is missing."
    | Some value -> 
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
            | _ -> Ok { 
                        MinLon = minLon
                        MinLat = Option.get parts.[1]
                        MaxLon = Option.get parts.[2]
                        MaxLat = Option.get parts.[3]
                    }

        let splits = value.Split (',') 

        match splits.Length with
        | 4 -> 
            let parsedSplits = splits |> Array.map tryParseFloat
            let hasAnyInvalidParts = parsedSplits |> Array.exists Option.isNone
            match hasAnyInvalidParts with
            | true -> 
                context |> withError "`bounds` parameter's value is invalid."
            | false ->
                let bounds = boundsFromParsedParts parsedSplits
                match bounds with
                | Error msg -> 
                    context 
                    |> withError ("`bounds` parameter's value is invalid, " + msg + ".")
                | Ok boundsVal -> 
                    let (_, oldOptions) = context
                    context 
                    |> withOptions ({ oldOptions with Bounds = Some boundsVal })
                    |> Result.Ok

        | _ -> context |> withError "`bounds` parameter's value is invalid."


let parseImportArgs (args: string list): ParsingResult =
    let defaultOptions = { 
        Bounds = None; SrtmDir = "srtm"; LocalCacheDir = "cache" }

    let mutable parsingResult: Result<ParsingContext, string> = 
        Ok (args, defaultOptions)

    while hasMoreArgsResult parsingResult do
        let (arg, context) = nextArgResult parsingResult

        parsingResult <-
            match arg with
            | Some "--bounds" -> parseBounds context
            | Some unknownArg ->
                Error (sprintf "Unknown argument '%s'." unknownArg)
            | None -> invalidOp "BUG: this should never happen"

    match parsingResult with
    | Ok (_, finalOptions) ->
        match finalOptions.Bounds with
        | None -> Error "`bounds` parameter is missing."
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
    (tiles: SrtmTileHgtFile seq)
    (readTile: SrtmTileReader)
    (createPngTileFile: string -> Stream)
    (encodeSrtmTileToPng: SrtmToPngEncoder)
    : unit = 

    for tileFile in tiles do
        let heightsArray = readTile tileFile
        let tileId = Demeton.Srtm.tileId tileFile.TileCoords

        use pngStream = createPngTileFile tileId
        encodeSrtmTileToPng heightsArray pngStream

    ignore()