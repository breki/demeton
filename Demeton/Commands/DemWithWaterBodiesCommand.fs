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
open Raster


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
       |> Option.desc
           "The DEM resolution (in meters) of the resulting DEM tile cell (default is 30 meters)."
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

let fetchAw3dTile
    (tileId: DemTileId)
    (localCacheDir: string)
    : Result<HeightsArray, string> =
    ensureAw3dTile
        localCacheDir
        FileSys.fileExists
        FileSys.downloadFile
        FileSys.readZipFile
        FileSys.copyStreamToFile
        FileSys.deleteFile
        tileId
    |> Result.bind (fun _ -> (readAw3dTile localCacheDir tileId) |> Result.Ok)




// todo 50: in order to properly identify the water bodies, we need to
//  work with the area that is larger than just that one 1x1 degree tile
let fetchWorldCoverTile
    (tileId: DemTileId)
    (localCacheDir: string)
    : HeightsArray =
    let containingTileId = containingWorldCoverFileTileId tileId

    // calculate the rectangle that covers the tileId
    let tileMinX, tileMinY = tileMinCell WorldCoverCellsPerDegree tileId

    let tile1by1Rect =
        { MinX = tileMinX
          MinY = tileMinY
          Width = WorldCoverCellsPerDegree
          Height = WorldCoverCellsPerDegree }

    ensureWorldCoverFile
        localCacheDir
        FileSys.fileExists
        FileSys.downloadFile
        containingTileId
    |> ignore


    readWorldCoverTiffFile
        localCacheDir
        (Some tile1by1Rect)
        containingTileId


let identifyAndSimplifyWaterBodies
    demResolutionParameter
    worldCoverHeightsArray
    =
    // first calculate how many pixels per degree will the final DEM have
    let finalDemPixelsPerDegree =
        float demResolutionParameter / 90. * (float Aw3dTileSize)

    // now, based on that value calculate the downsampling factor needed to
    // reduce WorldCover's 12000 degrees per degree to the final resolution
    let downsamplingFactor =
        finalDemPixelsPerDegree / (float WorldCoverCellsPerDegree)

    let worldCoverHighResolution =
        worldCoverHeightsArray |> convertWorldCoverRasterToWaterMonochrome

    let worldCoverTargetResolution =
        worldCoverHighResolution
        |> downsampleWaterBodiesHeightsArray downsamplingFactor

    worldCoverTargetResolution |> colorWaterBodies


let run (options: Options) : Result<unit, string> =
    fetchAw3dTile options.TileId options.LocalCacheDir
    |> Result.map (fun aw3dTile ->
        fetchWorldCoverTile options.TileId options.LocalCacheDir
        |> identifyAndSimplifyWaterBodies options.DemResolution
        |> ignore)

