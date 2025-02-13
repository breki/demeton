[<RequireQualifiedAccess>]
module Demeton.Commands.DemWithWaterBodiesCommand

open System
open System.IO
open CommandLine
open CommandLine.Common
open Demeton.Dem
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

type Options =
    { TileId: DemTileId
      TileSize: int
      LocalCacheDir: string }

[<Literal>]
let TileIdParameter = "tile-id"

[<Literal>]
let TileSizeParameter = "tile-size"

[<Literal>]
let LocalCacheDirParameter = "local-cache-dir"

[<Literal>]
let DefaultLocalCacheDir = "cache"

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

       Option.build TileSizeParameter
       |> Option.desc
           "The width/height of the resulting XTH tile (default is 3600)."
       |> Option.asPositiveInt
       |> Option.defaultValue 3600
       |> Option.toPar

       Option.build LocalCacheDirParameter
       |> Option.desc
           "The path to the local DEM cache directory. The directory will be created if it does not exist yet."
       |> Option.asDirectory
       |> Option.defaultValue DefaultLocalCacheDir
       |> Option.toPar |]

let fillOptions parsedParameters =
    let defaultOptions =
        { TileId = demTileXYId 0 0
          TileSize = 3600
          LocalCacheDir = DefaultLocalCacheDir }

    let processParameter options parameter =
        match parameter with
        | ParsedArg { Name = TileIdParameter
                      Value = value } ->
            { options with
                TileId = value :?> DemTileId }
        | ParsedOption { Name = TileSizeParameter
                         Value = value } ->
            { options with
                TileSize = value :?> int }
        | ParsedOption { Name = LocalCacheDirParameter
                         Value = value } ->
            { options with
                LocalCacheDir = value :?> string }
        | _ -> invalidOp "Unrecognized parameter."

    parsedParameters |> List.fold processParameter defaultOptions

let fetchAw3dTile
    (tileId: DemTileId)
    (localCacheDir: string)
    : Result<HeightsArray, string> =
    ensureAw3dTile
        localCacheDir
        FileSys.fileExists
        FileSys.downloadFileWithoutRedirects
        FileSys.readZipFile
        FileSys.copyStreamToFile
        FileSys.deleteFile
        FileSys.openFileToWrite
        tileId
    |> Result.bind (fun _ -> (readAw3dTile localCacheDir tileId) |> Result.Ok)


let downsampleAw3dTile finalTileSize heightsArray =
    let downsamplingFactor = float finalTileSize / float Aw3dTileSize

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


let downsampleWaterBodiesHeightsArray finalTileSize heightsArray =
    let downsamplingFactor =
        float finalTileSize / (float WorldCoverCellsPerDegree)

    heightsArray |> downsampleWaterBodiesHeightsArray downsamplingFactor

let identifyAndSimplifyWaterBodies finalTileSize worldCoverHeightsArray =
    let worldCoverHighResolution =
        worldCoverHeightsArray |> convertWorldCoverRasterToWaterMonochrome

    let worldCoverTargetResolution =
        worldCoverHighResolution
        |> downsampleWaterBodiesHeightsArray finalTileSize

    worldCoverTargetResolution |> colorWaterBodies


/// <summary>
/// Encodes the water bodies information into the DEM heights array.
/// </summary>
/// <remarks>
/// It uses the LSB of the height to encode the water bodies information.
/// If the bit is set to 1, the cell represents water. If it is set to 0,
/// the cell represents a land. This also means that all the odd-value heights
/// are rounded to the even number (e.g. 1 -> 0, 3 -> 2, 5 -> 4, etc.).
/// </remarks>
let encodeWaterBodiesInfoIntoDem
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

        demHeightsArray.Cells.[index] <-
            (height - Math.Abs(height % 2s)) + waterBody

    demHeightsArray


let extendHeightsArrayWithAdditionalRowAndColumn (heightsArray: HeightsArray) =
    let oldWidth = heightsArray.Width
    let newWidth = oldWidth + 1
    let newHeight = heightsArray.Height + 1

    let extendedCells = Array.zeroCreate (newWidth * newHeight)

    let mutable destIndex = 0

    for sourceIndex in 0 .. heightsArray.Cells.Length - 1 do
        extendedCells[destIndex] <- heightsArray.Cells.[sourceIndex]

        if (sourceIndex + 1) % oldWidth = 0 then
            // fill the final column with the "missing value" marker
            extendedCells.[destIndex + 1] <- DemHeightNone
            destIndex <- destIndex + 2
        else
            destIndex <- destIndex + 1

    // fill the final row with the "missing value" marker
    while destIndex < extendedCells.Length do
        extendedCells.[destIndex] <- DemHeightNone
        destIndex <- destIndex + 1

    HeightsArray(
        heightsArray.MinX,
        heightsArray.MinY,
        newWidth,
        newHeight,
        HeightsArrayDirectImport extendedCells
    )

let ensureXthFile cacheDir xthSize tileId : HeightsArray option =
    let xthFileName =
        Path.Combine(
            cacheDir,
            "AW3D-WaterBodies",
            xthSize.ToString(),
            toTileName tileId + ".xth"
        )

    if FileSys.fileExists xthFileName then
        // Log.info "The XTH file already exists at '%s'." xthFileName

        FileSys.openFileToRead xthFileName
        |> function
            | Ok stream ->
                use stream = stream
                Some(Xth.readHeightsArrayFromStream xthSize tileId stream)
            | Error error -> raise error.Exception
    else
        fetchAw3dTile tileId cacheDir
        |> Result.map (fun aw3dTile ->
            let availableWorldCoverTiles =
                ensureGeoJsonFile
                    cacheDir
                    FileSys.fileExists
                    FileSys.downloadFile
                |> listAllAvailableFiles FileSys.openFileToRead
                |> Set.ofSeq

            let downsampledAw3dTile = aw3dTile |> downsampleAw3dTile xthSize

            loadWaterBodiesTileFromCache
                cacheDir
                availableWorldCoverTiles
                tileId
            |> makeNoneFileIfNeeded cacheDir
            |> extractWaterBodiesTileFromWorldCoverTileIfNeeded cacheDir
            |> function
                | CachedTileLoaded tileHeightsArray ->
                    // todo 20: simplify water bodies

                    tileHeightsArray
                    |> Option.map (fun tileHeightsArray ->
                        let downsampledWaterBodiesTile =
                            tileHeightsArray
                            |> downsampleWaterBodiesHeightsArray xthSize

                        let demWaterBodiesTile =
                            downsampledAw3dTile
                            |> encodeWaterBodiesInfoIntoDem
                                downsampledWaterBodiesTile
                        // extend the heights array with one additional
                        //  row and column so it is compatible with the original
                        //  XTH files
                        // |> extendHeightsArrayWithAdditionalRowAndColumn

                        demWaterBodiesTile
                        |> Xth.writeHeightsArrayToFile xthFileName
                        |> ignore

                        demWaterBodiesTile)
                | TileNeedsToBeDownloaded _ ->
                    invalidOp "Bug: this should never happen"
                | TileDoesNotExistInWorldCover _ -> None)
        |> function
            | Ok heightsArray -> heightsArray
            | Error errorMessage -> invalidOp errorMessage


let run (options: Options) : Result<unit, string> =
    ensureXthFile options.LocalCacheDir options.TileSize options.TileId
    |> ignore

    Ok()
