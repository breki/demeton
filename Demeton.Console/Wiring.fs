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

let writePngTile = 
    encodeHeightsArrayIntoPngFile
        FileSys.ensureDirectoryExists
        FileSys.openFileToWrite

let convertPngTile = 
    convertZippedHgtTileToPng 
        FileSys.openZipFileEntry
        createSrtmTileFromStream
        writePngTile

let resampleHeightsArray: HeightsArrayResampler =
    invalidOp "todo"

let fetchSrtmTile srtmDir localCacheDir = 
    fetchSrtmTile 
        srtmDir
        localCacheDir
        FileSys.fileExists
        readPngTile
        writePngTile
        convertPngTile
        resampleHeightsArray

let fetchSrtmHeights srtmDir localCacheDir = 
    fetchSrtmHeights (fetchSrtmTile srtmDir localCacheDir)