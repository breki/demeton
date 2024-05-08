[<RequireQualifiedAccess>]
module Png.Rgba8Bit

open Raster

open FParsec
open System

[<Literal>]
let BytesPerPixel = 4

[<Literal>]
let TransparentColor = 0u

type RgbaColor = uint32
type ArgbColor = uint32

let inline r (color: RgbaColor) = byte (color >>> 24)
let inline g (color: RgbaColor) = byte (color >>> 16)
let inline b (color: RgbaColor) = byte (color >>> 8)
let inline a (color: RgbaColor) = byte color

/// <summary>
/// Constructs the <see cref="RgbaColor" /> from R, G, B and alpha
/// components.
/// </summary>
let inline rgbaColor (r: byte) (g: byte) (b: byte) (a: byte) : RgbaColor =
    (uint32 r <<< 24) ||| (uint32 g <<< 16) ||| (uint32 b <<< 8) ||| (uint32 a)

/// <summary>
/// Constructs a fully opaque <see cref="RgbaColor" /> from R, G and B
/// components.
/// </summary>
let inline rgbColor (r: byte) (g: byte) (b: byte) : RgbaColor =
    (uint32 r <<< 24) ||| (uint32 g <<< 16) ||| (uint32 b <<< 8) ||| 255u

let inline withAlpha (alpha: byte) (color: RgbaColor) : RgbaColor =
    (color &&& 0xffffff00u) ||| uint32 alpha

let inline toArgb (color: RgbaColor) : ArgbColor =
    (uint32 (a color) <<< 24)
    ||| (uint32 (r color) <<< 16)
    ||| (uint32 (g color) <<< 8)
    ||| (uint32 (b color))

let inline toHex (color: RgbaColor) : string =
    let alpha = a color

    match alpha with
    | 0xffuy -> $"#%02x{r color}%02x{g color}%02x{b color}"
    | _ -> $"#%02x{alpha}%02x{r color}%02x{g color}%02x{b color}"

/// <summary>
/// FParsec function for parsing color hex triplets and quadruplets into
/// <see cref="Rgba8Bit.RgbaColor" /> colors.
/// </summary>
let hexColor: Parser<RgbaColor, unit> =
    let hexCharToUInt c =
        let cInt = (uint32 c)

        match c with
        | x when x >= '0' && x <= '9' -> cInt - (uint32 '0')
        | x when x >= 'a' && x <= 'f' -> cInt - (uint32 'a') + 10u
        | x when x >= 'A' && x <= 'F' -> cInt - (uint32 'A') + 10u
        | _ -> invalidOp "Invalid hex character."

    let calcHex digits =
        digits
        |> Seq.toArray
        |> Array.fold (fun sum digit -> sum * 16u + (digit |> hexCharToUInt)) 0u

    pstring "#" >>. manyMinMaxSatisfyL 6 6 isHex "hex color value"
    .>>. opt (hex .>>. hex)
    |>> fun (digits, additionalDigits) ->
        let alphaDigits, rgbDigits =
            match additionalDigits with
            | Some(digit7, digit8) ->
                (digits.Substring(0, 2),
                 digits.Substring(2, 4) + string digit7 + string digit8)
            | None -> ("FF", digits)

        let alpha = calcHex alphaDigits
        let rgb = calcHex rgbDigits

        rgb <<< 8 ||| alpha

/// <summary>
/// Tries to parse a color hex triplet or quadruplet (with '#' prefix) into
/// <see cref="Rgba8Bit.RgbaColor" /> color.
/// </summary>
let tryParseColorHexValue
    hexValue
    : Result<RgbaColor, TextParsers.ParsingError> =
    match run hexColor hexValue with
    | Success(result, _, _) -> Result.Ok result
    | Failure(_, parserError, _) -> TextParsers.formatParsingFailure parserError


/// <summary>
/// Parses a color hex triplet or quadruplet (with '#' prefix) into
/// <see cref="Rgba8Bit.RgbaColor" /> color. If the hex value is invalid,
/// throws an <see cref="ArgumentException" />.
/// </summary>
/// <param name="hexValue"></param>
let parseColorHexValue hexValue : RgbaColor =
    match tryParseColorHexValue hexValue with
    | Result.Ok color -> color
    | Result.Error error -> raise (ArgumentException(error.Message))

let inline mixColors colorA colorB mixRatio =
    let mixByteValues (v1: byte) (v2: byte) : byte =
        let v1Float = float v1
        let mixedFloat = (float v2 - v1Float) * mixRatio + v1Float
        byte mixedFloat

    match mixRatio with
    | 0. -> colorA
    | 1. -> colorB
    | _ ->
        rgbaColor
            (mixByteValues (r colorA) (r colorB))
            (mixByteValues (g colorA) (g colorB))
            (mixByteValues (b colorA) (b colorB))
            (mixByteValues (a colorA) (a colorB))

let colorDistance colorA colorB =
    sqrt (
        Math.Pow(float (r colorA) - float (r colorB), 2.)
        + Math.Pow(float (g colorA) - float (g colorB), 2.)
        + Math.Pow(float (b colorA) - float (b colorB), 2.)
        + Math.Pow(float (a colorA) - float (a colorB), 2.)
    )

type ImageDataInitializer =
    | ImageDataZero
    | ImageDataInitializer1D of (int -> RgbaColor)
    | ImageDataInitializer2D of (int -> int -> RgbaColor)

let createImageData
    imageWidth
    imageHeight
    (initializer: ImageDataInitializer)
    : RawImageData =

    let imageDataSizeInBytes = imageWidth * imageHeight * BytesPerPixel
    let imageData = Array.zeroCreate imageDataSizeInBytes
    let mutable byteIndex = 0

    match initializer with
    | ImageDataZero -> ()
    | ImageDataInitializer2D initializer2D ->
        for y in 0 .. imageHeight - 1 do
            for x in 0 .. imageWidth - 1 do
                let pixelValue = initializer2D x y

                imageData.[byteIndex] <- r pixelValue
                imageData.[byteIndex + 1] <- g pixelValue
                imageData.[byteIndex + 2] <- b pixelValue
                imageData.[byteIndex + 3] <- a pixelValue
                byteIndex <- byteIndex + BytesPerPixel

    | ImageDataInitializer1D initializer1D ->
        let mutable pixelIndex = 0

        while byteIndex < imageDataSizeInBytes do
            let pixelValue = initializer1D pixelIndex

            imageData.[byteIndex] <- r pixelValue
            imageData.[byteIndex + 1] <- g pixelValue
            imageData.[byteIndex + 2] <- b pixelValue
            imageData.[byteIndex + 3] <- a pixelValue
            byteIndex <- byteIndex + BytesPerPixel
            pixelIndex <- pixelIndex + 1

    imageData


let pixelAt (imageData: byte[]) imageWidth x y : RgbaColor =
    let pixelIndex = x * BytesPerPixel + y * imageWidth * BytesPerPixel
    let r = imageData.[pixelIndex]
    let g = imageData.[pixelIndex + 1]
    let b = imageData.[pixelIndex + 2]
    let a = imageData.[pixelIndex + 3]
    rgbaColor r g b a

let setPixelAt
    (imageData: byte[])
    imageWidth
    x
    y
    (pixelValue: RgbaColor)
    : unit =
    let byteIndex = x * BytesPerPixel + y * imageWidth * BytesPerPixel
    imageData.[byteIndex] <- r pixelValue
    imageData.[byteIndex + 1] <- g pixelValue
    imageData.[byteIndex + 2] <- b pixelValue
    imageData.[byteIndex + 3] <- a pixelValue
