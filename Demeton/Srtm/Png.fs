module Demeton.Srtm.Png

open Demeton.DemTypes
open Png.Types
open Png.PixelFormats
open Png.File
open Demeton.Srtm.Types
open System.IO


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
    : HeightsArray =
    // todo write tests for it
    use stream = openFile pngFileName
    let (ihdr, imageData) = stream |> loadPngFromStream

    let tileId = pngFileName |> Pth.fileNameWithoutExtension
    let tileCoords = Tile.parseTileId tileId
    let (minX, minY) = Tile.tileCellMinCoords 3600 tileCoords

    HeightsArray(minX, minY, 3600, 3600,
        (fun (gx, gy) -> 
            let lx = gx - minX
            let ly = gy - minY
            let pixelValue = 
                grayscale16BitPixel imageData ihdr.Width ihdr.Height lx ly
            uint16ValueToDemHeight pixelValue))


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