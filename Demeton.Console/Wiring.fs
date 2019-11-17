[<RequireQualifiedAccess>]
module Demeton.Console.Wiring

open Demeton.Srtm.Types
open Demeton.Srtm.Funcs
open Demeton.Srtm.Png
open Demeton.Srtm.Fetch

let readPngTile: SrtmPngTileReader = 
    decodeSrtmTileFromPngFile FileSys.openFileToRead

let determineTileStatus srtmDir localCacheDir =
    determineTileStatus srtmDir localCacheDir FileSys.fileExists

let writePngTile = 
    writeHeightsArrayIntoPngFile
        FileSys.ensureDirectoryExists
        FileSys.openFileToWrite

let convertPngTile = 
    convertZippedHgtTileToPng 
        FileSys.openZipFileEntry
        createSrtmTileFromStream
        writePngTile

let fetchSrtmTile srtmDir localCacheDir
    : SrtmTileReader = fun tile ->

    let constructHigherLevelTile =
        constructHigherLevelTileHeightsArray 
            3600 localCacheDir readPngTile

    let heightsArrayToPng =
        writeHeightsArrayIntoPngFile
            FileSys.ensureDirectoryExists
            FileSys.openFileToWrite

    let writeTileToCache = 
        writeSrtmTileToLocalCache 
            localCacheDir
            heightsArrayToPng
            FileSys.openFileToWrite

    initializeProcessingState tile
    |> processCommandStack 
        localCacheDir srtmDir 
        (determineTileStatus srtmDir localCacheDir)
        convertPngTile 
        constructHigherLevelTile writeTileToCache
    |> finalizeFetchSrtmTileProcessing localCacheDir readPngTile

let fetchSrtmHeights srtmDir localCacheDir = 
    fetchSrtmHeights (fetchSrtmTile srtmDir localCacheDir)