module Demeton.Srtm.Png

open Demeton.DemTypes
open Png.Types
open Png.File
open Png.PixelFormats
open Demeton.Srtm.Types
open System.IO

let missingHeightAsUint16 = 0us
let zeroHeight = 1s <<< 15

let inline demHeightToUInt16Value (demHeight: DemHeight): uint16 =
    match demHeight with
    | DemHeightNone -> missingHeightAsUint16
    | height -> (uint16)((int16)height + zeroHeight)


let inline uint16ValueToDemHeight (value: uint16): DemHeight =
    if value = missingHeightAsUint16 then DemHeightNone
    else DemHeight ((int16)value - zeroHeight)

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

    let initializer = 
        Grayscale16BitImageDataInitializer1D(
            fun index -> heightsArray.Cells.[index] |> heightMappingFunc)

    grayscale16BitImageData
        heightsArray.Width
        heightsArray.Height
        initializer


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

    let imageData = heightsArrayToImageData demHeightToUInt16Value heightsArray 

    let ihdr = { 
        Width = heightsArray.Width
        Height = heightsArray.Height
        BitDepth = PngBitDepth.BitDepth16
        ColorType = PngColorType.Grayscale
        InterlaceMethod = PngInterlaceMethod.NoInterlace
        }

    outputStream 
    |> savePngToStream ihdr imageData


let encodeHeightsArrayIntoPngFile
    (ensureDirectoryExists: string -> string)
    openFile
    heightsArray 
    pngFileName =

    ensureDirectoryExists (pngFileName |> Pth.directory) |> ignore

    use stream = openFile pngFileName
    encodeSrtmHeightsArrayToPng heightsArray stream |> ignore


let decodeSrtmTileFromPngFile
    openFile
    pngFileName
    : Result<HeightsArray, string> =
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
        let tileId = pngFileName |> Pth.fileNameWithoutExtension
        let tileCoords = Tile.parseTileId tileId
        let (minX, minY) = Tile.tileCellMinCoords 3600 tileCoords

        let srtmTileInitialize = 
            HeightsArrayInitializer1D (fun index -> 
                let pixelIndex = index <<< 1
                let highByte = uint16 imageData.[pixelIndex]
                let lowByte = uint16 imageData.[pixelIndex + 1]
                let pixelValue = highByte <<< 8 ||| lowByte
                uint16ValueToDemHeight pixelValue)

        Ok (HeightsArray(minX, minY, 3600, 3600, srtmTileInitialize))

    let validationResult =
        ResultSeq.fold [ validateColorType; validateImageSize ] ihdr

    match validationResult with
    | Ok _ -> generateHeightsArray()
    | Error errors -> Error (errors |> String.concat " ")


let convertZippedHgtTileToPng
    (readZipFileEntry: FileSys.ZipFileEntryReader)
    createSrtmTileFromStream
    encodeTileIntoPngFile
    (zippedHgtFile: SrtmTileFile)
    pngFileName =
    
    let zippedEntryName = Tile.tileId zippedHgtFile.TileCoords + ".hgt"

    let zipEntryStream = 
        readZipFileEntry zippedHgtFile.FileName zippedEntryName

    let heightsArray =
        createSrtmTileFromStream 
            3600 zippedHgtFile.TileCoords zipEntryStream

    encodeTileIntoPngFile heightsArray pngFileName

    heightsArray