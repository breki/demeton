module Demeton.WaterBodies.Png


open System
open System.IO
open Demeton.Dem.Types
open Png.Types
open Raster
open Demeton.Dem.Funcs
open Demeton.WaterBodies.Types
open FileSys


/// <summary>
/// Encodes the <see cref="HeightsArray" /> containing water bodies tile data into
/// a PNG image and writes it into the specified stream.
/// </summary>
/// <param name="heightsArray">
/// A <see cref="HeightsArray" /> that holds the water bodies data for a tile.
/// </param>
/// <param name="outputStream">
/// The output stream into which the PNG image will be written.
/// </param>
/// <returns>This same instance of the output stream.</returns>
let encodeWaterBodiesHeightsArrayToPng
    (heightsArray: WaterBodiesHeightsArray)
    (outputStream: Stream)
    : Stream =

    let inline demHeight01ToBool demHeight =
        match demHeight with
        | 0s -> false
        | 1s -> true
        | _ -> raise <| ArgumentOutOfRangeException("demHeight")

    let imageData =
        heightsArrayToGrayscale1BitImageData demHeight01ToBool heightsArray

    let ihdr =
        { Width = heightsArray.Width
          Height = heightsArray.Height
          BitDepth = PngBitDepth.BitDepth1
          ColorType = PngColorType.Grayscale
          InterlaceMethod = PngInterlaceMethod.NoInterlace }

    outputStream |> Png.File.savePngToStream ihdr imageData


let decodeWaterBodiesTileFromPngStream
    tileSize
    tileId
    stream
    : Result<WaterBodiesHeightsArray, string> =
    let validateImageSize (ihdr: IhdrData) =
        match ihdr.Width, ihdr.Height with
        | tileWidth, tileHeight when
            tileWidth = tileSize && tileHeight = tileSize
            ->
            Ok ihdr
        | _, _ ->
            Error
                "The image size of this PNG does not correspond to the water bodies tile."

    let validateColorType (ihdr: IhdrData) =
        match ihdr.ColorType with
        | PngColorType.Grayscale -> Ok ihdr
        | _ ->
            Error
                "The color type of this PNG does not correspond to the water bodies tile."

    let validateBitDepth (ihdr: IhdrData) =
        match ihdr.BitDepth with
        | PngBitDepth.BitDepth1 -> Ok ihdr
        | _ ->
            Error
                "The bit depth of this PNG does not correspond to the water bodies tile."

    let generateHeightsArray (ihdr: IhdrData) (imageData: RawImageData) =
        let minX, minY = tileMinCell tileSize tileId

        let waterBodiesTileInitialize (cells: DemHeight[]) =
            let totalPixelsCount = ihdr.Width * ihdr.Height

            let mutable pixelIndex = 0
            let mutable byteIndex = 0
            let mutable bitMask = 1uy <<< 7

            while pixelIndex < totalPixelsCount do
                let pixelValue = imageData.[byteIndex] &&& bitMask > 0uy

                cells.[pixelIndex] <- if pixelValue then 1s else 0s

                pixelIndex <- pixelIndex + 1
                bitMask <- bitMask >>> 1

                if pixelIndex % ihdr.Width = 0 then
                    byteIndex <- byteIndex + 1
                    bitMask <- 1uy <<< 7
                elif bitMask = 0uy then
                    bitMask <- 1uy <<< 7
                    byteIndex <- byteIndex + 1

        Ok(
            WaterBodiesHeightsArray(
                minX,
                minY,
                tileSize,
                tileSize,
                HeightsArrayCustomInitializer waterBodiesTileInitialize
            )
        )

    let ihdr, imageData = stream |> Png.File.loadPngFromStream

    stream |> closeStream

    let validationResult =
        ResultSeq.fold
            [ validateColorType; validateBitDepth; validateImageSize ]
            ihdr

    match validationResult with
    | Ok _ -> generateHeightsArray ihdr imageData
    | Error errors -> Error(errors |> String.concat " ")


let decodeWaterBodiesTileFromPngFile
    tileSize
    tileId
    fileName
    : Result<WaterBodiesHeightsArray, string> =
    openFileToRead fileName
    |> function
        | Ok stream ->
            stream |> decodeWaterBodiesTileFromPngStream tileSize tileId
        | Error error -> Error error.Exception.Message
