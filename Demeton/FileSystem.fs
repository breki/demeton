module FileSystem

open System.IO

type FileExistsChecker = string -> bool

type ZipFileEntryReader = string -> string -> Stream