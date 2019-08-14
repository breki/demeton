// Learn more about F# at http://fsharp.org

open System
open System.IO;
open System.IO.Compression;

let hgtFileName (demFile : string) = demFile + ".hgt"

let hgtZipFileName (demFile : string) = hgtFileName demFile + ".zip"

let unzippedHgtFileName (demOutputDir : string) (demFile : string) = 
    let hgtFileNameShort = demFile.Substring(0, 7)
    Path.Combine (demOutputDir, hgtFileNameShort) + ".hgt"

[<EntryPoint>]
let main argv =
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

