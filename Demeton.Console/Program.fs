open System
open System.IO;
open System.IO.Compression;
open Demeton.Srtm
open Demeton.HgtPng
open Demeton.Png
open System.Reflection

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


[<EntryPoint>]
let main argv =
    match argv.[0] with
    | "hgt" -> downloadAndUnzipHgtFile()
    | "encode-png" -> encodePng argv.[1]
    | "decode-png" -> decodePng argv.[1]
    | _ -> 0
