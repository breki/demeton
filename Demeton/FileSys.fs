/// <summary>
/// Contains functions for working with files and directories 
/// in a functional way.
/// </summary>
[<RequireQualifiedAccess>]
module FileSys

open System.IO
open System.IO.Compression

/// <summary>
/// A function providing the ability open a reading stream to a file entry 
/// inside a ZIP package.
/// </summary>
type ZipFileEntryReader = string -> string -> Stream

/// <summary>
/// A function providing the ability to check whether a file exists or not.
/// </summary>
type FileExistsChecker = string -> bool

/// <summary>
/// Determines whether the specified file exists.
/// </summary>
let fileExists: FileExistsChecker = fun fileName -> File.Exists(fileName)

/// <summary>
/// Deletes a directory if it exists. If it does not exist, does nothing.
/// </summary>
let deleteDirectoryIfExists (directory: string): string =
    match Directory.Exists(directory) with
    | true -> Directory.Delete(directory, true)
    | false -> ignore()

    directory

/// <summary>
/// A function providing the ability to ensure the directory exists on the disk.
/// </summary>
type DirectoryExistsEnsurer = string -> string

/// <summary>
/// Creates the specified directory path if it does not exist already.
/// </summary>
let ensureDirectoryExists: DirectoryExistsEnsurer = 
    fun (directory: string) ->
    Directory.CreateDirectory(directory) |> ignore
    directory

/// <summary>
/// A function providing the ability to open a file stream.
/// </summary>
type FileOpener = string -> Stream

/// <summary>
/// Opens a read stream to the specified file.
/// </summary>
let openFileToRead: FileOpener = 
    fun fileName -> File.OpenRead(fileName) :> Stream

/// <summary>
/// Opens a write stream to the specified file.
/// </summary>
let openFileToWrite: FileOpener = 
    fun fileName -> File.OpenWrite(fileName) :> Stream

/// <summary>
/// Open a reading stream to a file entry inside a ZIP package.
/// </summary>
let openZipFileEntry: ZipFileEntryReader = fun zipFileName entryName ->
    let zipArchive = ZipFile.OpenRead(zipFileName)
    let entry = zipArchive.GetEntry(entryName)
    entry.Open()
