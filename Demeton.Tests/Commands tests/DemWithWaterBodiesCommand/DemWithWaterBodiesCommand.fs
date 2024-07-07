[<RequireQualifiedAccess>]
module Demeton.Commands.DemWithWaterBodiesCommand

open System
open CommandLine
open CommandLine.Common
open Demeton.Dem.Types
open Demeton.Dem.Funcs

type Options =
    { TileId: DemTileCoords
      DemResolution: int
      LocalCacheDir: string
      OutputDir: string }

[<Literal>]
let TileIdParameter = "tile-id"

[<Literal>]
let DemResolutionParameter = "dem-resolution"

[<Literal>]
let LocalCacheDirParameter = "local-cache-dir"

[<Literal>]
let OutputDirParameter = "output-dir"

[<Literal>]
let DefaultLocalCacheDir = "cache"

[<Literal>]
let DefaultOutputDir = "output"

let parseTileId value : OptionValueParsingResult =
    try
        let demTileCoords = parseHgtTileName value
        OkValue demTileCoords
    with :? InvalidOperationException as ex ->
        InvalidValue ex.Message

let supportedParameters: CommandParameter[] =
    [| Arg.build TileIdParameter
       |> Arg.desc "The SRTM ID of the tile (example: 'N45E005')."
       |> Arg.parser parseTileId
       |> Arg.toPar

       Option.build DemResolutionParameter
       |> Option.desc "The DEM resolution (in meters) of the resulting DEM tile cell (default is 30 meters)."
       |> Option.asPositiveInt
       |> Option.defaultValue 30
       |> Option.toPar

       Option.build LocalCacheDirParameter
       |> Option.desc
           "The path to the local DEM cache directory. The directory will be created if it does not exist yet."
       |> Option.asDirectory
       |> Option.defaultValue DefaultLocalCacheDir
       |> Option.toPar

       Option.build OutputDirParameter
       |> Option.desc
           "The path to the directory where the raster files will be generated. The directory will be created if it does not exist yet."
       |> Option.asDirectory
       |> Option.defaultValue DefaultOutputDir
       |> Option.toPar |]

let fillOptions parsedParameters =
    let defaultOptions =
        { TileId =
            { Lon = { Value = 0 }
              Lat = { Value = 0 } }
          DemResolution = 30
          LocalCacheDir = DefaultLocalCacheDir
          OutputDir = DefaultOutputDir }

    let processParameter options parameter =
        match parameter with
        | ParsedArg { Name = TileIdParameter
                      Value = value } ->
            { options with
                TileId = value :?> DemTileCoords }
        | ParsedOption { Name = DemResolutionParameter
                         Value = value } ->
            { options with
                DemResolution = value :?> int }
        | ParsedOption { Name = LocalCacheDirParameter
                         Value = value } ->
            { options with
                LocalCacheDir = value :?> string }
        | ParsedOption { Name = OutputDirParameter
                         Value = value } ->
            { options with
                OutputDir = value :?> string }
        | _ -> invalidOp "Unrecognized parameter."

    parsedParameters |> List.fold processParameter defaultOptions


let run (options: Options) : Result<unit, string> = Result.Ok()
