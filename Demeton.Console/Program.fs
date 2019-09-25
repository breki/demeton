open System
open System.IO;
open System.IO.Compression;
open Demeton.Srtm
open Demeton.HgtPng
open Png.File
open Demeton.Commands.ImportSrtmTilesCommand

let hgtFileName (demFile : string) = demFile + ".hgt"

let hgtZipFileName (demFile : string) = hgtFileName demFile + ".zip"

let unzippedHgtFileName (demOutputDir : string) (demFile : string) = 
    let hgtFileNameShort = demFile.Substring(0, 7)
    Path.Combine (demOutputDir, hgtFileNameShort) + ".hgt"


let downloadAndUnzipHgtFile() =
    let DemSourceDir = @"\\hobbit\SRTM"
    let DemOutputDir = "d:\dem"
    let DemFile = "N00E010.SRTMGL1"

    let dirInfo = Directory.CreateDirectory DemOutputDir

    ZipFile.ExtractToDirectory(
        Path.Combine(DemSourceDir, hgtZipFileName(DemFile)),
        DemOutputDir,
        true)

    let outputFileName = unzippedHgtFileName DemOutputDir DemFile

    let outputFileExists = File.Exists(outputFileName)
    if not outputFileExists then 
        let message = 
            sprintf
                "The HGT file %s was downloaded and unzipped"
                outputFileName
        raise(InvalidOperationException message)

    printfn "The HGT file %s was downloaded and unzipped" outputFileName

    0

let encodePng (hgtFileName: string) = 
    let srtmTileId = Path.GetFileNameWithoutExtension (hgtFileName)

    let tileCoords = parseTileId srtmTileId

    use hgtStream = File.OpenRead hgtFileName

    let clock = new System.Diagnostics.Stopwatch()
    clock.Start()

    printfn ("Reading the heights array...")
    
    let heightsArray = createSrtmTileFromStream 3600 tileCoords hgtStream

    let pngFileName = Path.GetFullPath(srtmTileId + ".png")
    use pngWriteStream = File.OpenWrite(pngFileName)
    
    printfn
        "%d Encoding heights into the PNG %s ..." clock.ElapsedMilliseconds pngFileName

    encodeSrtmHeightsArrayToPng heightsArray pngWriteStream |> ignore
    pngWriteStream.Close()

    printfn "%d DONE." clock.ElapsedMilliseconds

    0


let decodePng (pngFileName: string) = 
    let clock = new System.Diagnostics.Stopwatch()
    clock.Start()
    
    use pngReadStream = File.OpenRead (pngFileName)

    printfn "Decoding the PNG..."

    let (ihdrRead, imageDataRead) = 
        pngReadStream |> loadPngFromStream 

    printfn "%d DONE." clock.ElapsedMilliseconds

    0

let displayHelp exitCode = 
    // todo: add code to display all the available commands
    printfn "We'll display help text here soon..."
    exitCode

let handleUnknownCommand commandName =
    printfn "Unknown command '%s'." commandName
    1


let importTiles options =
    let tilesCords = boundsToTiles (Option.get options.Bounds) |> Seq.toArray

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
