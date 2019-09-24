/// <summary>
/// Contains functions for working with files and directories 
/// in a functional way.
/// </summary>
module FileSystem

open System.IO
open System.IO.Compression

type FileExistsChecker = string -> bool

type ZipFileEntryReader = string -> string -> Stream

let deleteDirectoryIfExists (directory: string): string =
    match Directory.Exists(directory) with
    | true -> Directory.Delete(directory, true)
    | false -> ignore()

    directory

let ensureDirectoryExists (directory: string): string =
    Directory.CreateDirectory(directory) |> ignore
    directory

let openFileToRead fileName = File.OpenRead(fileName)

let openFileToWrite fileName = File.OpenWrite(fileName)

let openZipFileEntry zipFileName entryName =
    let zipArchive = ZipFile.OpenRead(zipFileName)
    let entry = zipArchive.GetEntry(entryName)
    entry.Open()