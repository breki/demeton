module Demeton.Srtm.Png

open Demeton.DemTypes
open Raster
open Png
open Png.Types
open Png.File
open Demeton.Srtm.Funcs
open System.IO
open Types

[<Literal>]
let MissingHeightAsUint16 = 0us
[<Literal>]
let ZeroHeight = -32768s

let inline demHeightToUInt16Value (demHeight: DemHeight): uint16 =
    match demHeight with
    | DemHeightNone -> MissingHeightAsUint16
    | height -> (uint16)((int16)height + ZeroHeight)


let inline uint16ValueToDemHeight (value: uint16): DemHeight =
    if value = MissingHeightAsUint16 then DemHeightNone
    else DemHeight ((int16)value - ZeroHeight)

/// <summary>
/// Converts the <see cref="HeightsArray" /> into 16-bit grayscale image data.
/// </summary>
/// <param name="heightsArray">
/// A <see cref="HeightsArray" /> that holds heights data to be converted.
/// </param>
/// <returns>Image data.</returns>
let heightsArrayToImageData 
    (heightMappingFunc: DemHeight -> uint16)
    (heightsArray: HeightsArray)
    : RawImageData =

    let inline initializer index = 
        heightsArray.Cells.[index] |> heightMappingFunc

    Grayscale16Bit.createImageData
        heightsArray.Width
        heightsArray.Height
        (Grayscale16Bit.ImageDataInitializer1D initializer)


/// <summary>
/// Encodes the <see cref="HeightsArray" /> containing SRTM tile data into
/// a PNG image and writes it into the specified stream.
/// </summary>
/// <param name="heightsArray">
/// A <see cref="HeightsArray" /> that holds heights data of the SRTM tile.
/// </param>
/// <param name="outputStream">
/// The output stream into which the PNG image will be written.
/// </param>
/// <returns>This same instance of the output stream.</returns>
let encodeSrtmHeightsArrayToPng 
    (heightsArray: HeightsArray)
    (outputStream: Stream): Stream =

    let imageData = 
        heightsArrayToImageData demHeightToUInt16Value heightsArray 

    let ihdr = { 
        Width = heightsArray.Width
        Height = heightsArray.Height
        BitDepth = PngBitDepth.BitDepth16
        ColorType = PngColorType.Grayscale
        InterlaceMethod = PngInterlaceMethod.NoInterlace
        }

    outputStream 
    |> savePngToStream ihdr imageData


let writeHeightsArrayIntoPngFile
    (ensureDirectoryExists: string -> string)
    (openFileToWrite: FileSys.FileOpener): HeightsArrayPngWriter =
    fun pngFileName heightsArray ->

    ensureDirectoryExists (pngFileName |> Pth.directory) |> ignore

    use stream = openFileToWrite pngFileName
    encodeSrtmHeightsArrayToPng heightsArray stream |> ignore

    heightsArray

/// <summary>
/// Writes a tile into a local cache as a PNG file.
/// </summary>
type SrtmTileCacheWriter = SrtmTileCoords -> HeightsArray option -> unit

/// <summary>
/// Writes a tile into a local cache as a PNG file. If the supplied 
/// height array is None and the tile level is above 0, writes a ".none" file
/// instead.
/// </summary>
let writeSrtmTileToLocalCache
    (localCacheDir: FileSys.DirectoryName)
    (writeHeightsArrayToFile: HeightsArrayPngWriter)
    (openFileToWrite: FileSys.FileOpener)
    : SrtmTileCacheWriter =
    fun (tile: SrtmTileCoords)
        (heightsArrayMaybe: HeightsArray option) -> 
    match (heightsArrayMaybe, tile.Level.Value) with
    | (Some heightsArray, _) ->
        let pngFileName = tile |> toLocalCacheTileFileName localCacheDir
        writeHeightsArrayToFile pngFileName heightsArray |> ignore
    | (None, 0) -> ignore()
    | (None, _) -> 
        let noneFileName = 
            tile |> toLocalCacheTileFileName localCacheDir
            |> Pth.extension ".none"
        let stream = openFileToWrite noneFileName
        stream.Close()
        ignore()

let decodeSrtmTileFromPngFile
    (openFile: FileSys.FileOpener): SrtmPngTileReader =
    fun tileCoords pngFileName ->
    Log.info "Loading PNG SRTM tile '%s'..." pngFileName

    use stream = openFile pngFileName

    let (ihdr, imageData) = stream |> loadPngFromStream

    let validateImageSize ihdr =
        match (ihdr.Width, ihdr.Height) with
        | (3600, 3600) -> Ok ihdr
        | (_, _) -> 
            Error "The image size of this PNG does not correspond to the SRTM tile."

    let validateColorType (ihdr: IhdrData) =
        match ihdr.ColorType with
        | PngColorType.Grayscale -> Ok ihdr
        | _ -> 
            Error "The color type of this PNG does not correspond to the SRTM tile."

    let generateHeightsArray() = 
        let (minX, minY) = Tile.tileCellMinCoords 3600 tileCoords

        let srtmTileInitialize (cells: DemHeight[]) = 
            let mutable byteIndex = 0
            let imageSize = ihdr.Width * ihdr.Height

            for index in 0 .. imageSize - 1 do
                let highByte = uint16 imageData.[byteIndex]
                let lowByte = uint16 imageData.[byteIndex + 1]
                let pixelValue = highByte <<< 8 ||| lowByte
                cells.[index] <- uint16ValueToDemHeight pixelValue
                byteIndex <- byteIndex + 2

        Ok (HeightsArray(minX, minY, 3600, 3600, 
                HeightsArrayCustomInitializer srtmTileInitialize))

    let validationResult =
        ResultSeq.fold [ validateColorType; validateImageSize ] ihdr

    match validationResult with
    | Ok _ -> generateHeightsArray()
    | Error errors -> Error (errors |> String.concat " ")

/// <summary>
/// Reads a SRTM tile from the zipped HGT file and saves it into a PNG file.
/// </summary>
/// <remarks>
/// Note that, although the function nominally returns a Result, it currently
/// does not handle any errors/exceptions by itself, so it always returns Ok.
/// </remarks>
let convertZippedHgtTileToPng
    (readZipFileEntry: FileSys.ZipFileEntryReader)
    (createSrtmTileFromStream: ZippedSrtmTileReader)
    (writeHeightsArrayIntoPng: HeightsArrayPngWriter)
    : SrtmHgtToPngTileConverter =
    fun tileCoords zippedHgtFileName pngFileName ->
    
    let tileId = Tile.tileId tileCoords
    let zippedEntryName = tileId + ".hgt"

    let zipEntryStream = 
        readZipFileEntry zippedHgtFileName zippedEntryName

    Log.debug "Reading tile %s..." zippedEntryName

    let heightsArray =
        createSrtmTileFromStream 3600 tileCoords zipEntryStream

    Log.debug "Encoding tile %s into PNG..." tileId

    writeHeightsArrayIntoPng pngFileName heightsArray |> Ok
