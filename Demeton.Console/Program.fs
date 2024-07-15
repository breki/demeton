open CommandLine
open CommandLine.Common
open Demeton.Commands
open Demeton.Console
open Demeton.Dem.Funcs
open Demeton.Shaders.Types
open System

let runImportCommand parsedParameters =
    let options = ImportSrtmTilesCommand.fillOptions parsedParameters

    let tilesCords =
        boundsToTiles
            3600
            (Demeton.Dem.Types.DemLevel.fromInt 0)
            (Option.get options.Bounds)
        |> Seq.toArray

    ImportSrtmTilesCommand.run
        tilesCords
        (Wiring.determineTileStatus options.SrtmDir options.LocalCacheDir)
        (Wiring.fetchSrtmTile options.SrtmDir options.LocalCacheDir)

    CommandExecuted


/// <summary>
/// Runs the 'shade' command using the specified parsed parameters.
/// </summary>
let runShadeCommand parsedParameters =
    let options = ShadeCommand.fillOptions parsedParameters

    let generateTile =
        ShadeCommand.generateShadedRasterTile
            [| fun level coverageArea dataSources ->
                   Wiring.fetchSrtmHeights
                       options.SrtmDir
                       options.LocalCacheDir
                       level
                       coverageArea
                   |> heightsArrayResultToShadingDataSource
                       DefaultDataSourceKey
                       (Ok dataSources) |]
            Demeton.Shaders.Pipeline.Common.createShadingFuncById

    let saveTile =
        ShadeCommand.saveShadedRasterTile
            FileSys.ensureDirectoryExists
            FileSys.openFileToWrite
            Png.File.savePngToStream

    let results = ShadeCommand.run options generateTile saveTile

    match results with
    | Ok _ -> CommandExecuted
    | Error message ->
        Console.Error.WriteLine message
        CommandFailed


let runTileShadeCommand parsedParameters =
    let options = TileShadeCommand.fillOptions parsedParameters

    let results = TileShadeCommand.run options

    match results with
    | Ok _ -> CommandExecuted
    | Error message ->
        Console.Error.WriteLine message
        CommandFailed

let runDemWithWaterBodiesCommand parsedParameters =
    let options = DemWithWaterBodiesCommand.fillOptions parsedParameters

    let results = DemWithWaterBodiesCommand.run options

    match results with
    | Ok _ -> CommandExecuted
    | Error message ->
        Console.Error.WriteLine message
        CommandFailed

let supportedCommands: Command[] =
    [| { Name = "import"
         ShortDescription = "imports SRTM tiles into the local cache"
         Description =
           "Converts SRTM tiles from the original format (zipped SRTM HGT "
           + "files) to PNG files and stores them in the local cache, to be "
           + "available later for processing by the 'shade' command."
           + "This command is useful if you want to pre-cache a certain area "
           + "of the world and store it on another computer that has no "
           + "access to the full SRTM tiles archive. Note that the 'shade' "
           + "command also converts any SRTM tiles not found in the cache, so "
           + "the 'import' is just a convenience command."
         Parameters = ImportSrtmTilesCommand.supportedParameters
         Runner = runImportCommand }
       { Name = "shade"
         ShortDescription = "generates a shaded raster"
         Description =
           "Generates a shaded raster image (consisting of one or more image "
           + "tiles) for a given geographic area, using the specified map "
           + "projection, map scale and printing resolution."
         Parameters = ShadeCommand.supportedParameters
         Runner = runShadeCommand }
       { Name = "tile-shade"
         ShortDescription = "generates a shaded raster tile"
         Description =
           "Generates a shaded raster image tile using a special workflow."
         Parameters = TileShadeCommand.supportedParameters
         Runner = runTileShadeCommand }
       { Name = "dem-with-water-bodies"
         ShortDescription = "generates DEM with water bodies data"
         Description =
           "Generates a SRTM-like HGT file containing DEM and water bodies data."
         Parameters = DemWithWaterBodiesCommand.supportedParameters
         Runner = runDemWithWaterBodiesCommand } |]

let helpCommand =
    { Name = "help"
      ShortDescription = "displays help information (this command)"
      Description =
        "Displays the help information about the command line interface."
      Parameters = [||]
      Runner =
        HelpCommand.run
            "demeton"
            supportedCommands
            Console.Out.Write
            Console.Error.Write }


[<EntryPoint>]
let main args =
    Console.Out.WriteLine
        "Demeton by Igor Brejc (https://github.com/breki/demeton)"

    Console.Out.WriteLine ""

    let commandResult =
        Shell.parseAndExecuteCommandLine
            Console.Out.Write
            Console.Error.Write
            "demeton"
            args
            supportedCommands

    let exitCode =
        match commandResult with
        | CommandExecuted -> 0
        | CommandFailed -> 1
        | ParsingFailed -> 2
        | UnrecognizedCommand -> 3

    exit exitCode

    exitCode
