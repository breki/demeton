[<RequireQualifiedAccess>]
module Demeton.Console.Wiring

open Demeton.Srtm.Downsampling
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

let openHgtStream = openZippedHgtFileStream FileSys.openZipFileEntry

let convertPngTile = 
    convertZippedHgtTileToPng 
        openHgtStream createSrtmTileFromStream writePngTile

let fetchSrtmTile srtmDir localCacheDir
    : SrtmTileReader = fun tile ->

    let constructHigherLevelTile =
        constructHigherLevelTileHeightsArray 3600

    let heightsArrayToPng =
        writeHeightsArrayIntoPngFile
            FileSys.ensureDirectoryExists
            FileSys.openFileToWrite

    let writeTileToCache = 
        writeSrtmTileToLocalCache 
            localCacheDir
            FileSys.ensureDirectoryExists
            heightsArrayToPng
            FileSys.openFileToWrite

    initializeProcessingState tile
    |> processCommandStack 
        localCacheDir srtmDir 
        (determineTileStatus srtmDir localCacheDir)
        readPngTile
        convertPngTile 
        constructHigherLevelTile writeTileToCache
    |> finalizeFetchSrtmTileProcessing

let fetchSrtmHeights srtmDir localCacheDir = 
    fetchSrtmHeights (fetchSrtmTile srtmDir localCacheDir)