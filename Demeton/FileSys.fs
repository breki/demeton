/// <summary>
/// Contains functions for working with files and directories
/// in a functional way.
/// </summary>
module FileSys

open System
open System.IO
open System.IO.Compression
open System.Net.Http

type FileName = string
type DirectoryName = string

type FileSysError = { Exception: Exception }

let inline fileSysErrorMessage error = error.Exception.Message

let mapFileSysException (ex: Exception) =
    match ex with
    | :? DirectoryNotFoundException as ex -> Some { Exception = ex }
    | :? FileNotFoundException as ex -> Some { Exception = ex }
    | :? PathTooLongException as ex -> Some { Exception = ex }
    | :? IOException as ex -> Some { Exception = ex }
    | :? UnauthorizedAccessException as ex -> Some { Exception = ex }
    | _ -> None

/// <summary>
/// A function providing the ability open a reading stream to a file entry
/// inside a ZIP package.
/// </summary>
type ZipFileReader<'TResult> =
    string
        -> string
        -> (Stream -> Result<'TResult, FileSysError>)
        -> Result<'TResult, FileSysError>

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
let deleteDirectoryIfExists (directory: string) : Result<string, FileSysError> =
    try
        match Directory.Exists(directory) with
        | true -> Directory.Delete(directory, true)
        | false -> ()

        directory |> Ok
    with ex ->
        match mapFileSysException ex with
        | Some error -> Error error
        | None -> raise ex


let deleteFile (fileName: string) : Result<string, FileSysError> =
    try
        File.Delete(fileName)
        Ok fileName
    with ex ->
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
        with ex ->
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
let openFileToRead: FileReader =
    fun fileName ->
        try
            File.OpenRead(fileName) :> Stream |> Ok
        with ex ->
            match mapFileSysException ex with
            | Some error -> Error error
            | None -> raise ex


/// <summary>
/// Opens a write stream to the specified file.
/// </summary>
let openFileToWrite: FileWriter =
    fun fileName ->
        try
            File.OpenWrite(fileName) :> Stream |> Ok
        with ex ->
            match mapFileSysException ex with
            | Some error -> Error error
            | None -> raise ex

/// <summary>
/// Open a reading stream to a file entry inside a ZIP package and call the
/// provided stream reading function.
/// </summary>
let readZipFile: ZipFileReader<'TResult> =
    fun zipFileName entryName streamReader ->
        try
            use zipArchive = ZipFile.OpenRead(zipFileName)
            let entry = zipArchive.GetEntry(entryName)

            match entry with
            | null ->
                sprintf
                    "ZIP file '%s' does not have a file entry '%s'."
                    zipFileName
                    entryName
                |> FileNotFoundException
                |> raise

            | _ ->
                use entryStream = entry.Open()
                streamReader entryStream
        with ex ->
            match mapFileSysException ex with
            | Some error -> Error error
            | None -> raise ex

/// An experimental function for different way of working with streams as
/// disposables.
let withFileRead fileName callback =
    try
        use stream = File.OpenRead(fileName)
        callback stream
    with ex ->
        match mapFileSysException ex with
        | Some error -> Error error
        | None -> raise ex

/// <summary>
/// Copies the content of the input stream to a file.
/// </summary>
let copyStreamToFile (fileName: string) (inputStream: Stream) =
    try
        use outputStream = new FileStream(fileName, FileMode.Create)
        inputStream.CopyTo(outputStream)
        Ok fileName
    with ex ->
        match mapFileSysException ex with
        | Some error -> Error error
        | None -> raise ex


let closeStream (stream: Stream) = stream.Close()

/// <summary>
/// Downloads a file from the specified URL and saves it to the specified
/// path on the disk.
/// </summary>
let downloadFile (url: string) (destinationPath: string): string =
    Log.debug $"Downloading file from %s{url} to %s{destinationPath}..."

    let httpClient = new HttpClient()

    let downloadTask =
        async {
            let! response = httpClient.GetAsync(url) |> Async.AwaitTask

            if response.IsSuccessStatusCode then
                let! content =
                    response.Content.ReadAsByteArrayAsync() |> Async.AwaitTask

                ensureDirectoryExists (Path.GetDirectoryName(destinationPath))
                |> ignore

                use fileStream =
                    new FileStream(destinationPath, FileMode.Create)

                fileStream.Write(content, 0, content.Length)

                return destinationPath
            else
                return
                    failwithf
                        "Failed to download file from %s. Status code: %s"
                        url
                        (response.StatusCode.ToString())
        }

    downloadTask |> Async.RunSynchronously
