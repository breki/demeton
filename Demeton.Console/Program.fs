open Demeton.Srtm.Funcs
open Demeton.Srtm.Png
open Demeton.Commands

let displayHelp exitCode = 
    // todo: add code to display all the available commands
    printfn "We'll display help text here soon..."
    exitCode

let handleUnknownCommand commandName =
    printfn "Unknown command '%s'." commandName
    1


let importTiles (options: ImportSrtmTilesCommand.Options) =
    let tilesCords = boundsToTiles (Option.get options.Bounds) |> Seq.toArray

    let cachingStatusChecker =
        checkSrtmTileCachingStatus
            options.SrtmDir
            options.LocalCacheDir
            FileSys.fileExists

    let pngTileReader =
        decodeSrtmTileFromPngFile
            FileSys.openFileToRead

    let pngTileConverter = 
        convertZippedHgtTileToPng 
            FileSys.openZipFileEntry
            createSrtmTileFromStream
            (encodeHeightsArrayIntoPngFile
                FileSys.ensureDirectoryExists
                FileSys.openFileToWrite)

    ImportSrtmTilesCommand.run 
        tilesCords 
        cachingStatusChecker
        (fetchSrtmTile 
            options.SrtmDir
            options.LocalCacheDir
            FileSys.fileExists
            pngTileReader
            pngTileConverter)

    0


let shade options = 
    invalidOp "todo"
    //let readSrtmTile = 
    //    fetchSrtmTile
            

    //let generateTile =
    //    ShadeCommand.generateShadedRasterTile
    //        fetchSrtmHeights
    //        ShadeCommand.shadeRaster


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
