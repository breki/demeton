/// <summary>
/// Contains functions for working with files and directories 
/// in a functional way.
/// </summary>
module FileSystem

open System.IO
open System.IO.Compression

/// <summary>
/// A function providing the ability to check whether a file exists or not.
/// </summary>
type FileExistsChecker = string -> bool

/// <summary>
/// A function providing the ability open a reading stream to a file entry 
/// inside a ZIP package.
/// </summary>
type ZipFileEntryReader = string -> string -> Stream

/// <summary>
/// Deletes a directory if it exists. If it does not exist, does nothing.
/// </summary>
let deleteDirectoryIfExists (directory: string): string =
    match Directory.Exists(directory) with
    | true -> Directory.Delete(directory, true)
    | false -> ignore()

    directory

/// <summary>
/// Creates the specified directory path if it does not exist already.
/// </summary>
let ensureDirectoryExists (directory: string): string =
    Directory.CreateDirectory(directory) |> ignore
    directory

/// <summary>
/// Opens a read stream to the specified file.
/// </summary>
let openFileToRead fileName = File.OpenRead(fileName)

/// <summary>
/// Opens a write stream to the specified file.
/// </summary>
let openFileToWrite fileName = File.OpenWrite(fileName)

/// <summary>
/// Open a reading stream to a file entry inside a ZIP package.
/// </summary>
let openZipFileEntry zipFileName entryName =
    let zipArchive = ZipFile.OpenRead(zipFileName)
    let entry = zipArchive.GetEntry(entryName)
    entry.Open()