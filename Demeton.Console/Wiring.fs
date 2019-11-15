[<RequireQualifiedAccess>]
module Demeton.Console.Wiring

open Demeton.Srtm.Types
open Demeton.Srtm.Funcs
open Demeton.Srtm.Png
open Demeton.Srtm.Fetch

let readPngTile: SrtmPngTileReader = 
    decodeSrtmTileFromPngFile FileSys.openFileToRead

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

let fetchSrtmTile srtmDir localCacheDir
    : SrtmTileReader = fun tile ->

    let determineTileStatus = 
        determineTileStatus srtmDir localCacheDir FileSys.fileExists

    let constructHigherLevelTile =
        constructHigherLevelTileHeightsArray 
            3600 localCacheDir readPngTile

    initializeProcessingState tile
    |> processCommandStack 
        localCacheDir srtmDir 
        determineTileStatus convertPngTile constructHigherLevelTile
    |> finalizeFetchSrtmTileProcessing localCacheDir readPngTile

let fetchSrtmHeights srtmDir localCacheDir = 
    fetchSrtmHeights (fetchSrtmTile srtmDir localCacheDir)