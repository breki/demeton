﻿/// <summary>
/// Contains type definitions for the PNG reading and writing functionality.
/// </summary>
/// <remarks>
/// https://en.wikipedia.org/wiki/Portable_Network_Graphics
/// https://www.w3.org/TR/PNG/#11IHDR
/// http://www.libpng.org/pub/png/book/chapter08.html
/// https://www.w3.org/TR/REC-png-961001
/// </remarks>
module Png.Types

/// <summary>
/// A single-byte integer giving the number of bits per sample or per palette
/// index (not per pixel).
/// </summary>
type PngBitDepth =
    | BitDepth1 = 1uy
    | BitDepth2 = 2uy
    | BitDepth4 = 4uy
    | BitDepth8 = 8uy
    | BitDepth16 = 16uy

/// <summary>
/// A single-byte integer that defines the PNG image type.
/// </summary>
type PngColorType =
    | Grayscale = 0uy
    | Rgb = 2uy
    | Indexed = 3uy
    | GrayscaleAlpha = 4uy
    | RgbAlpha = 6uy

/// <summary>
/// A single-byte integer that indicates the method used to compress the image
/// data.
/// </summary>
type PngCompressionMethod =
    | DeflateInflate = 0uy

/// <summary>
/// A single-byte integer that indicates the preprocessing method applied to the
/// image data before compression.
/// </summary>
type PngFilterMethod =
    | AdaptiveFiltering = 0uy

/// <summary>
/// Defines the type of PNG adaptive filtering operation performed for a
/// specific image row (scanline).
/// </summary>
type FilterType =
    | FilterNone = 0uy
    | FilterSub = 1uy
    | FilterUp = 2uy
    | FilterAverage = 3uy
    | FilterPaeth = 4uy

/// <summary>
/// A single-byte integer that indicates the transmission order of the image
/// data.
/// </summary>
type PngInterlaceMethod =
    | NoInterlace = 0uy
    | Adam7Interlace = 1uy

/// <summary>
/// A four-character name of the PNG chunk type which specifies the function of
/// the chunk.
/// </summary>
[<Struct>]
type ChunkType =
    val TypeName: string

    new(typeName: string) =
        { TypeName =
            if typeName.Length <> 4 then
                invalidArg
                    "typeName"
                    "PNG chunk type must be 4 characters long."
            else
                typeName }

/// <summary>
/// The record type containing IHDR (image header) chunk data.
/// </summary>
type IhdrData =
    {
        /// <summary>
        /// Image width in pixels.
        /// </summary>
        Width: int
        /// <summary>
        /// Image height in pixels.
        /// </summary>
        Height: int
        /// <summary>
        /// The number of bits per sample or per palette index (not per pixel).
        /// </summary>
        BitDepth: PngBitDepth
        /// <summary>
        /// Defines the PNG image type.
        /// </summary>
        ColorType: PngColorType
        /// <summary>
        /// Indicates the transmission order of the image data.
        /// </summary>
        InterlaceMethod: PngInterlaceMethod
    }

    /// <summary>
    /// Calculates the number of bits per pixel needed for this instance of
    /// IHDR data.
    /// </summary>
    /// <remarks>
    /// Not all combinations of color type and bit depth are currently
    /// supported. If you need a new combination, please implement it.
    /// </remarks>
    member this.BitsPerPixel =
        match (this.ColorType, this.BitDepth) with
        | PngColorType.Grayscale, PngBitDepth.BitDepth1 -> 1
        | PngColorType.Grayscale, PngBitDepth.BitDepth8 -> 8
        | PngColorType.Grayscale, PngBitDepth.BitDepth16 -> 16
        | PngColorType.RgbAlpha, PngBitDepth.BitDepth8 -> 8 * 4
        | _, _ -> invalidOp "This PNG type is currently not supported."

/// <summary>
/// A byte array representing a sequence of filtered scanlines of the image.
/// </summary>
type FilteredImageData = byte[]

/// <summary>
/// A filtered row of pixels within an image, represented as a byte array.
/// </summary>
type FilteredScanline = byte[]


type ScanlineBitDepthMode =
    | SubByteMode of int
    | ByteMode of int
