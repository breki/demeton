[<RequireQualifiedAccess>]
module Demeton.Console.Wiring

open Demeton.Srtm.Funcs
open Demeton.Srtm.Png

let readPngTile = decodeSrtmTileFromPngFile FileSys.openFileToRead

let checkCachingStatus srtmDir localCacheDir =
    checkSrtmTileCachingStatus
        srtmDir
        localCacheDir
        FileSys.fileExists

let convertPngTile = 
    convertZippedHgtTileToPng 
        FileSys.openZipFileEntry
        createSrtmTileFromStream
        (encodeHeightsArrayIntoPngFile
            FileSys.ensureDirectoryExists
            FileSys.openFileToWrite)

let fetchSrtmTile srtmDir localCacheDir = 
    fetchSrtmTile 
        srtmDir
        localCacheDir
        FileSys.fileExists
        readPngTile
        convertPngTile

let fetchSrtmHeights srtmDir localCacheDir = 
    fetchSrtmHeights (fetchSrtmTile srtmDir localCacheDir)