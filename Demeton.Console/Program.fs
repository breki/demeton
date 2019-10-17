open CommandLine
open CommandLine.Common
open Demeton.Srtm.Funcs
open Demeton.Commands
open Demeton.Console

let displayHelp exitCode = 
    // todo: add code to display all the available commands
    printfn "We'll display help text here soon..."
    exitCode

let handleUnknownCommand commandName =
    printfn "Unknown command '%s'." commandName
    1


let runImportCommand parsedParameters =
    let options = ImportSrtmTilesCommand.fillOptions parsedParameters

    let tilesCords = boundsToTiles (Option.get options.Bounds) |> Seq.toArray

    ImportSrtmTilesCommand.run 
        tilesCords 
        (Wiring.checkCachingStatus
            options.SrtmDir
            options.LocalCacheDir)
        (Wiring.fetchSrtmTile options.SrtmDir options.LocalCacheDir)

    CommandExecuted


let runShadeCommand parsedParameters = 
    let options = ShadeCommand.fillOptions parsedParameters

    let generateTile =
        ShadeCommand.generateShadedRasterTile
            (Wiring.fetchSrtmHeights options.SrtmDir options.LocalCacheDir)
            ShadeCommand.rasterShaderFactory

    let saveTile =
        ShadeCommand.saveShadedRasterTile
            FileSys.ensureDirectoryExists
            FileSys.openFileToWrite
            Png.File.savePngToStream

    let results =
        ShadeCommand.run options generateTile saveTile
    
    CommandExecuted


let supportedCommands: Command[] = [|
    {
        Name = "import";
        ShortDescription = "imports SRTM tiles into the local cache"
        Parameters = ImportSrtmTilesCommand.supportedParameters
        Runner = runImportCommand };
    {
        Name = "shade";
        ShortDescription = "generates a shaded raster"
        Parameters = ShadeCommand.supportedParameters
        Runner = runShadeCommand };
|]

let helpCommand = {
    Name = "help";
    ShortDescription = "displays help information (this command)";
    Parameters = [| |]
    Runner = HelpCommand.runCommand 
        "demeton" supportedCommands System.Console.Out.Write };


[<EntryPoint>]
let main args =
    let commandResult = 
        Shell.parseAndExecuteCommandLine 
            System.Console.Out.Write "demeton" args supportedCommands

    match commandResult with
    | CommandExecuted -> 0
    | ParsingFailed -> 1
    | CommandNotFound -> 2
