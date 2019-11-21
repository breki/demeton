/// <summary>
/// Contains functions for working with files and directories 
/// in a functional way.
/// </summary>
module FileSys

open System
open System.IO
open System.IO.Compression

type FileName = string
type DirectoryName = string

type FileSysError = {
    Exception: System.Exception
}

let inline fileSysErrorMessage error = error.Exception.Message

let mapFileSysException (ex: Exception) =
    match ex with
    | :? DirectoryNotFoundException as ex -> Some { Exception =  ex }
    | :? FileNotFoundException as ex -> Some { Exception =  ex }
    | :? PathTooLongException as ex -> Some { Exception =  ex }
    | :? IOException as ex -> Some { Exception =  ex }
    | :? UnauthorizedAccessException as ex -> Some { Exception =  ex }
    | _ -> None

/// <summary>
/// A function providing the ability open a reading stream to a file entry 
/// inside a ZIP package.
/// </summary>
type ZipFileEntryReader = string -> string -> Result<Stream, FileSysError>

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
let deleteDirectoryIfExists (directory: string): Result<string, FileSysError> =
    try
        match Directory.Exists(directory) with
        | true -> Directory.Delete(directory, true)
        | false -> ignore()

        directory |> Ok
    with
    | ex ->
        match mapFileSysException ex with
        | Some error -> Error error
        | None -> raise ex


/// <summary>
/// A function providing the ability to ensure the directory exists on the disk.
/// </summary>
type DirectoryExistsEnsurer = string -> Result<string, FileSysError>

/// <summary>
/// Creates the specified directory path if it does not exist already.
/// </summary>
let ensureDirectoryExists: DirectoryExistsEnsurer = 
    fun (directory: string) ->
    try
        Directory.CreateDirectory(directory) |> ignore
        Ok directory
    with
    | ex ->
        match mapFileSysException ex with
        | Some error -> Error error
        | None -> raise ex

/// <summary>
/// A function providing the ability to open a file stream to read.
/// </summary>
type FileReader = string -> Result<Stream, FileSysError>

/// <summary>
/// A function providing the ability to open a file stream to write.
/// </summary>
type FileWriter = string -> Result<Stream, FileSysError>

/// <summary>
/// Opens a read stream to the specified file.
/// </summary>
let openFileToRead: FileReader = fun fileName ->
    try
        File.OpenRead(fileName) :> Stream |> Ok
        with
    | ex ->
        match mapFileSysException ex with
        | Some error -> Error error
        | None -> raise ex


/// <summary>
/// Opens a write stream to the specified file.
/// </summary>
let openFileToWrite: FileWriter = fun fileName ->
    try
        File.OpenWrite(fileName) :> Stream |> Ok
        with
    | ex ->
        match mapFileSysException ex with
        | Some error -> Error error
        | None -> raise ex

/// <summary>
/// Open a reading stream to a file entry inside a ZIP package.
/// </summary>
let openZipFileEntry: ZipFileEntryReader = fun zipFileName entryName ->
    try
        let zipArchive = ZipFile.OpenRead(zipFileName)
        let entry = zipArchive.GetEntry(entryName)
        match entry with
        | null ->
            sprintf
                "ZIP file '%s' does not have a file entry '%s'."
                zipFileName entryName
            |> FileNotFoundException
            |> raise
            
        | _ -> entry.Open() |> Ok
    with
    | ex ->
        match mapFileSysException ex with
        | Some error -> Error error
        | None -> raise ex

/// An experimental function for different way of working with streams as
/// disposables.
let withFileRead fileName callback =
    try
        use stream = File.OpenRead(fileName)
        callback stream
        with
    | ex ->
        match mapFileSysException ex with
        | Some error -> Error error
        | None -> raise ex

let closeStream (stream: Stream) =
    stream.Close()
