module Demeton.HgtPng

open DemTypes
open Demeton.PngTypes
open Demeton.PngPixelFormats
open Demeton.Png
open Demeton.SrtmTypes
open Demeton.Srtm
open System.IO
open FileSystem


let missingHeightAsUint16 = 0us
let zeroHeight = 1s <<< 15

let inline demHeightToUInt16Value (demHeight: DemHeight option): uint16 =
    match demHeight with
    | Some height -> (uint16)((int16)height + zeroHeight)
    | _ -> missingHeightAsUint16


let inline uint16ValueToDemHeight (value: uint16): DemHeight option =
    if value = missingHeightAsUint16 then None
    else Some (DemHeight ((int16)value - zeroHeight))


/// <summary>
/// Converts the <see cref="HeightsArray" /> into 16-bit grayscale image data.
/// </summary>
/// <param name="heightsArray">
/// A <see cref="HeightsArray" /> that holds heights data to be converted.
/// </param>
/// <returns>Image data.</returns>
let heightsArrayToImageData 
    (heightMappingFunc: DemHeight option -> uint16)
    (heightsArray: HeightsArray)
    : ImageData =

    grayscale16BitImageData
        heightsArray.Width
        heightsArray.Height
        (fun x y -> heightsArray.Cells.[x, y] |> heightMappingFunc)


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


let convertZippedHgtTileToPng
    (readZipFileEntry: ZipFileEntryReader)
    createSrtmTileFromStream
    (zippedHgtFile: SrtmTileFile)
    pngFileName =
    
    let zippedEntryName = tileId zippedHgtFile.TileCoords + ".hgt"

    let zipEntryStream = 
        readZipFileEntry zippedHgtFile.FileName zippedEntryName

    let heightsArray =
        createSrtmTileFromStream 
            3600 zippedHgtFile.TileCoords zipEntryStream

    ignore()