[<RequireQualifiedAccess>]
module Demeton.Commands.DemWithWaterBodiesCommand

open System
open CommandLine
open CommandLine.Common
open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Demeton.Aw3d.Types
open Demeton.Aw3d.Funcs
open Demeton.WaterBodies.Funcs
open Demeton.WorldCover.Types
open Demeton.WorldCover.Fetch
open Demeton.WorldCover.Funcs
open Demeton.WorldCover.Coloring
open Demeton.WorldCover.WaterBodiesFetch
open Raster
open FileSys
open System.IO

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


let downsampleAw3dTile demResolutionParameter heightsArray =
    let downsamplingFactor = float demResolutionParameter / 90.

    heightsArray |> downsampleHeightsArray downsamplingFactor


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


    readWorldCoverTiffFile localCacheDir (Some tile1by1Rect) containingTileId


let downsampleWaterBodiesHeightsArray demResolutionParameter heightsArray =
    // first calculate how many pixels per degree will the final DEM have
    let finalDemPixelsPerDegree =
        float demResolutionParameter / 90. * (float Aw3dTileSize)

    // now, based on that value calculate the downsampling factor needed to
    // reduce WorldCover's 12000 degrees per degree to the final resolution
    let downsamplingFactor =
        finalDemPixelsPerDegree / (float WorldCoverCellsPerDegree)

    heightsArray |> downsampleWaterBodiesHeightsArray downsamplingFactor

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


// todo 1: instead of putting Int16.MinValue for water bodies, use the LSB
//   (0 - no water body, 1  - water body) - test the behavior for negative values, too

let mergeDemWithWaterBodies
    (waterBodiesHeightsArray: HeightsArray)
    (demHeightsArray: HeightsArray)
    : HeightsArray =
    if
        waterBodiesHeightsArray.Width <> demHeightsArray.Width
        || waterBodiesHeightsArray.Height <> demHeightsArray.Height
    then
        invalidArg
            "waterBodiesHeightsArray"
            (sprintf
                "The water bodies array (%d,%d) must have the same dimensions as the DEM array (%d,%d)."
                waterBodiesHeightsArray.Width
                waterBodiesHeightsArray.Height
                demHeightsArray.Width
                demHeightsArray.Height)

    for index in 0 .. waterBodiesHeightsArray.Cells.Length - 1 do
        let height = demHeightsArray.Cells.[index]
        let waterBody = waterBodiesHeightsArray.Cells.[index]

        let zz = height % 2s
        demHeightsArray.Cells.[index] <- (height - Math.Abs(height % 2s)) + waterBody

    demHeightsArray


let writeHeightsArrayToHgtFile
    (fileName: FileName)
    (heightsArray: HeightsArray)
    : FileName =
    Log.debug "Writing the DEM tile to %s..." fileName

    fileName |> Pth.directory |> FileSys.ensureDirectoryExists |> ignore

    FileSys.openFileToWrite (fileName)
    |> function
        | Ok stream ->
            use stream = stream

            for height in heightsArray.Cells do
                stream |> Bnry.writeBigEndianInt16 height |> ignore

            stream.Close()

            fileName
        | Error error -> raise error.Exception

// todo 5: generate additional row and column in HGT file so it is compatible
//   with SRTM
let run (options: Options) : Result<unit, string> =
    fetchAw3dTile options.TileId options.LocalCacheDir
    |> Result.map (fun aw3dTile ->
        let availableWorldCoverTiles =
            ensureGeoJsonFile
                options.LocalCacheDir
                FileSys.fileExists
                FileSys.downloadFile
            |> listAllAvailableFiles FileSys.openFileToRead
            |> Set.ofSeq

        let downsampledAw3dTile =
            aw3dTile |> downsampleAw3dTile options.DemResolution

        loadWaterBodiesTileFromCache
            options.LocalCacheDir
            availableWorldCoverTiles
            options.TileId
        |> makeNoneFileIfNeeded options.LocalCacheDir
        |> extractWaterBodiesTileFromWorldCoverTileIfNeeded
            options.LocalCacheDir
        |> function
            | CachedTileLoaded tileHeightsArray ->
                // todo 20: simplify water bodies

                tileHeightsArray
                |> Option.map (fun tileHeightsArray ->
                    let downsampledWaterBodiesTile =
                        tileHeightsArray
                        |> downsampleWaterBodiesHeightsArray
                            options.DemResolution

                    let hgtFileName =
                        downsampledAw3dTile
                        |> mergeDemWithWaterBodies downsampledWaterBodiesTile
                        |> writeHeightsArrayToHgtFile (
                            Path.Combine(
                                options.OutputDir,
                                toTileName options.TileId + ".hgt"
                            )
                        )

                    hgtFileName)
            | TileNeedsToBeDownloaded _ ->
                invalidOp "Bug: this should never happen"
            | TileDoesNotExistInWorldCover _ ->
                NotImplementedException(
                    "The case when there is no WorldCover tile"
                )
                |> raise
        |> ignore

        ())
