[<RequireQualifiedAccess>]
module Demeton.Console.Wiring

open System
open BitMiracle.LibTiff.Classic
open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Demeton.Srtm.Downsampling
open Demeton.Srtm.Png
open Demeton.Srtm.Fetch

let readPngTile: DemPngTileReader =
    decodeSrtmTileFromPngFile FileSys.openFileToRead

let determineTileStatus srtmDir localCacheDir =
    determineTileStatus srtmDir localCacheDir FileSys.fileExists

let writePngTile =
    writeHeightsArrayIntoPngFile
        FileSys.ensureDirectoryExists
        FileSys.openFileToWrite

let readZippedHgtFile = readZippedHgtFile FileSys.readZipFile

let convertPngTile = convertZippedHgtTileToPng readZippedHgtFile writePngTile

let fetchSrtmTile srtmDir localCacheDir : DemTileReader =
    fun tile ->

        let constructHigherLevelTile =
            constructHigherLevelTileHeightsArray Demeton.Srtm.Funcs.SrtmTileSize

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
            localCacheDir
            srtmDir
            (determineTileStatus srtmDir localCacheDir)
            readPngTile
            convertPngTile
            constructHigherLevelTile
            writeTileToCache
        |> finalizeFetchSrtmTileProcessing

let fetchSrtmHeights srtmDir localCacheDir srtmLevel lonLatBounds =
    let tileReader = fetchSrtmTile srtmDir localCacheDir

    let srtmTilesNeeded =
        boundsToTiles Demeton.Srtm.Funcs.SrtmTileSize srtmLevel lonLatBounds

    fetchDemHeights tileReader srtmTilesNeeded




type DisableErrorHandler() =
    inherit TiffErrorHandler()

    override this.WarningHandler
        (tif: Tiff, method: string, format: string, [<ParamArray>] args: obj[]) =
        // do nothing, ie, do not write warnings to console
        ()

    override this.WarningHandlerExt
        (
            tif: Tiff,
            clientData: obj,
            method: string,
            format: string,
            [<ParamArray>] args: obj[]
        ) =
        // do nothing, ie, do not write warnings to console
        ()
