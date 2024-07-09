[<RequireQualifiedAccess>]
module Demeton.Commands.DemWithWaterBodiesCommand

open System
open CommandLine
open CommandLine.Common
open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Demeton.Aw3d.Types
open Demeton.Aw3d.Funcs
open Demeton.WorldCover.Types
open Demeton.WorldCover.Fetch
open Demeton.WorldCover.Funcs
open Demeton.WorldCover.WaterBodiesColoring

type Options =
    { TileId: DemTileId
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
        OkValue(demTileXYId demTileCoords.Lon.Value demTileCoords.Lat.Value)
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
        { TileId = demTileXYId 0 0
          DemResolution = 30
          LocalCacheDir = DefaultLocalCacheDir
          OutputDir = DefaultOutputDir }

    let processParameter options parameter =
        match parameter with
        | ParsedArg { Name = TileIdParameter
                      Value = value } ->
            { options with
                TileId = value :?> DemTileId }
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

let fetchAw3dTile (tileId: DemTileId) (localCacheDir: string) : Result<HeightsArray, string> =
    ensureAw3dTile
        localCacheDir
        FileSys.fileExists
        FileSys.downloadFile
        FileSys.readZipFile
        FileSys.copyStreamToFile
        FileSys.deleteFile
        tileId
    |> Result.bind (fun _ -> (readAw3dTile localCacheDir tileId) |> Result.Ok)


let fetchWorldCoverTile (tileId: DemTileId) (localCacheDir: string) : Result<HeightsArray, string> =
    let containingTileId = containingWorldCoverFileTileId tileId
    ensureWorldCoverFile localCacheDir FileSys.fileExists FileSys.downloadFile containingTileId
    |> Result.bind (fun _ -> (readWorldCoverTiffFile localCacheDir containingTileId) |> Result.Ok)


// todo 8: we probably need a special downsampling function since a) we need
//   to be able to specify the downsampled width, height and b) we do not calculate
//   averages of heights, we instead only look if there is a water body in the
//   downsampled area or not
let downsampleHeightsArrayWithFloatFactor (downsamplingFactor: float) (originalArray: HeightsArray)  : HeightsArray =
    let originalWidth = originalArray.Width
    let originalHeight = originalArray.Height
    let newWidth = int (float originalWidth / factor)
    let newHeight = int (float originalHeight / factor)
    let downsampledArray = Array2D.zeroCreate<float> newHeight newWidth

    for y in 0 .. newHeight - 1 do
        for x in 0 .. newWidth - 1 do
            let origX = float x * factor
            let origY = float y * factor
            downsampledArray.[y, x] <- bilinearInterpolate origX origY originalArray

    downsampledArray

// todo 3: this takes a long time since we're working on the original,
//   high-resolution data. Maybe we should downsample the heights array to the
//   final resolution first?
let identifyAndSimplifyWaterBodies worldCoverHeightsArray =
    worldCoverHeightsArray
    |> convertWorldCoverRasterToWaterMonochrome
    |> colorWaterBodies


let run (options: Options) : Result<unit, string> =
    fetchAw3dTile options.TileId options.LocalCacheDir
    |> Result.bind (fun aw3dTile ->
        fetchWorldCoverTile options.TileId options.LocalCacheDir
        |> Result.bind (
            fun worldCoverTile ->
                identifyAndSimplifyWaterBodies worldCoverTile |> ignore
                Result.Ok()))
