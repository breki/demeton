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


let shade (options: ShadeCommand.Options) = 
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
    
    0


let parseArgsAndRun (args: string[]) =
    match args.Length with
    | 0 -> displayHelp(0)
    | _ -> 
        match args.[0] with
        | "shade" ->
            let parseResult = 
                args |> Array.toList |> List.tail |> ShadeCommand.parseArgs 

            match parseResult with
            | Ok parsedParameters -> invalidOp "todo"//shade options
            | Error errMessage -> 
                printfn "Parsing error: %s" errMessage
                1
            
        | x -> handleUnknownCommand x


let supportedCommands: Command[] = [|
    {
        Name = "import";
        Parameters = ImportSrtmTilesCommand.supportedParameters
        Runner = runImportCommand }
|]

[<EntryPoint>]
let main args =
    match parseAndExecuteCommandLine args supportedCommands with
    | CommandExecuted -> 0
    | ParsingFailed -> 1
    | CommandNotFound -> 2
