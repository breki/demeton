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


let importTiles (options: ImportSrtmTilesCommand.Options) =
    let tilesCords = boundsToTiles (Option.get options.Bounds) |> Seq.toArray

    ImportSrtmTilesCommand.run 
        tilesCords 
        (Wiring.checkCachingStatus
            options.SrtmDir
            options.LocalCacheDir)
        (Wiring.fetchSrtmTile options.SrtmDir options.LocalCacheDir)

    0


let shade (options: ShadeCommand.Options) = 
    let generateTile =
        ShadeCommand.generateShadedRasterTile
            (Wiring.fetchSrtmHeights options.SrtmDir options.LocalCacheDir)
            ShadeCommand.shadeRaster

    invalidOp "todo"
    //ShadeCommand.run 
    //    options
    //    generateTile
    //    saveTile
    
    //0


let parseArgsAndRun (args: string[]) =
    match args.Length with
    | 0 -> displayHelp(0)
    | _ -> 
        match args.[0] with
        | "import" -> 
            let parseResult = 
                args |> Array.toList |> List.tail 
                |> ImportSrtmTilesCommand.parseArgs 

            match parseResult with
            | Ok (_, options) -> importTiles options
            | Error errMessage -> 
                printfn "Parsing error: %s" errMessage
                1
        | "shade" ->
            let parseResult = 
                args |> Array.toList |> List.tail |> ShadeCommand.parseArgs 

            match parseResult with
            | Ok (_, options) -> shade options
            | Error errMessage -> 
                printfn "Parsing error: %s" errMessage
                1
            
        | x -> handleUnknownCommand x


[<EntryPoint>]
let main args =
    parseArgsAndRun args
