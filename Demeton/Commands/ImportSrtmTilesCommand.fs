module Demeton.Commands.ImportSrtmTilesCommand

open Demeton.GeometryTypes    
open Demeton.SrtmTypes
open Demeton.DemTypes
open System
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


let parseBounds (context: ParsingContext) =
    match nextArg context with
    | None -> context |> withError "`bounds` parameter's value is missing."
    | Some value -> 
        let splits = value.Split (',') |> Array.map (fun s -> Double.Parse s)
    
        match splits.Length with
        | 4 -> 
            let (args, options) = context
            Ok (args,  { options with Bounds = Some { MinLon = splits.[0]; MinLat = splits.[1]; MaxLon = splits.[2]; MaxLat = splits.[3] }})
        | _ -> context |> withError "Invalid bounds value"


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