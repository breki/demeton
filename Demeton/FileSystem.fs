module FileSystem

open System.IO
open System.IO.Compression

type FileExistsChecker = string -> bool

type ZipFileEntryReader = string -> string -> Stream

let openZipFileEntry zipFileName entryName =
    let zipArchive = ZipFile.OpenRead(zipFileName)
    let entry = zipArchive.GetEntry(entryName)
    entry.Open()