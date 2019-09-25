open Demeton.Srtm.Funcs
open Demeton.Srtm.Png
open Demeton.Commands.ImportSrtmTilesCommand

let displayHelp exitCode = 
    // todo: add code to display all the available commands
    printfn "We'll display help text here soon..."
    exitCode

let handleUnknownCommand commandName =
    printfn "Unknown command '%s'." commandName
    1


let importTiles options =
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

    import 
        tilesCords 
        cachingStatusChecker
        (fetchSrtmTile 
            options.SrtmDir
            options.LocalCacheDir
            FileSys.fileExists
            pngTileReader
            pngTileConverter)

    0


let parseArgsAndRun (args: string[]) =
    match args.Length with
    | 0 -> displayHelp(0)
    | _ -> 
        match args.[0] with
        | "import" -> 
            let parseResult = 
                args |> Array.toList |> List.tail |> parseImportArgs 

            match parseResult with
            | Ok (_, options) -> importTiles options
            | Error errMessage -> 
                printfn "Parsing error: %s" errMessage
                1
        | x -> handleUnknownCommand x


[<EntryPoint>]
let main args =
    parseArgsAndRun args
