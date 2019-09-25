module Png.Filters

open Png.Types

open System
open System.Threading.Tasks

let bytesPerPixel bpp =
    let bytes = bpp / 8
    let bitRemainder = bpp % 8
    match (bytes, bitRemainder) with
    | (_, 0) -> bytes
    | (0, _) -> 
        invalidOp "Bit depths less than 8 bits are currently not supported."
    | (_, _) -> 
        invalidOp "Invalid bpp (bits-per-pixel) value."


let inline paethPredictor (a:int) (b:int) (c:int) = 
    let p = a + b - c
    let pa = abs (p - a)
    let pb = abs (p - b)
    let pc = abs (p - c)
    match (pa, pb, pc) with
    | (_, _, _) when pa <= pb && pa <= pc -> a
    | (_, _, _) when pb <= pc -> b
    | _ -> c


/// <summary>
/// Converts a filtered scaline back to the original scanline using the 
/// "Paeth" PNG filter method.
/// </summary>
/// <param name="bpp">Bits per pixel of the image.</param>
/// <param name="prevScanline">
/// The previous (upper) scanline that is used to calculate filter values.
/// </param>
/// <param name="filtered">
/// The filtered scanline that should be converted.
/// </param>
/// <returns>The original (non-filtered) scanline.</returns>


let inline filterTypeNone raw = raw

let inline filterTypeSub raw left _ _ = raw - left

let inline filterTypeUp raw _ up _ = raw - up

let inline filterTypeAverage raw (left: byte) (up: byte) _ 
    = raw - (byte)(((int)left + (int)up) / 2)

let inline filterTypeAverageFirstLine raw (left: byte)
    = raw - byte ((int left) / 2)

let inline filterTypePaeth raw (left: byte) (up: byte) (upLeft: byte) = 
    raw - (byte)(paethPredictor ((int)left) ((int)up) ((int)upLeft))

let inline filterTypePaethFirstLine raw (left: byte) = raw - left


let scanlineFromImageData imageData scanlineLength index: Span<byte> =
    new Span<byte>(imageData, index * scanlineLength, scanlineLength)

type ScanlineFilter = ImageData -> int -> int -> int -> int -> FilteredScanline

let addFilterTypeByteMarks(filteredScanlinesBuffer: FilteredScanline[]) = 
    for filterIndex in 0 .. 4 do
        filteredScanlinesBuffer.[filterIndex].[0] <- (byte)filterIndex


let createFilteredScanlinesBuffer filteredScanlineLength =
    let filteredScanlinesBuffer: FilteredScanline[] = 
        Array.init 
            5 
            (fun _ -> Array.zeroCreate filteredScanlineLength)

    addFilterTypeByteMarks filteredScanlinesBuffer

    filteredScanlinesBuffer


let filteredScanlineWithMinSumOfAbsDiffs 
    (filteredScanlinesBuffer: FilteredScanline[]) 
    (sumsOfAbsDiffs: int[]) =
    
    let filterTypesCount = 5
    let mutable minIndex = -1
    let mutable minValueSoFar = System.Int32.MaxValue

    for i in 0 .. (filterTypesCount - 1) do
        let minValueOfFilter = sumsOfAbsDiffs.[i]
        if minValueOfFilter < minValueSoFar then
            minIndex <- i
            minValueSoFar <- minValueOfFilter
        else ignore()

    filteredScanlinesBuffer.[minIndex]


let filterFirstScanline
    imageData
    (bytesPP: int) 
    scanlineLength 
    (filteredScanlinesBuffer: FilteredScanline[]) =
    // there are five types of PNG adaptive filters
    let filterTypesCount = 5

    let scanline = 
        scanlineFromImageData imageData scanlineLength 0

    let sumsOfAbsDiffs: int[] = Array.zeroCreate filterTypesCount

    // For None and Up, simply copy the original scanline bytes unfiltered.
    // This works for Up because there is no "up" scanline.
    scanline.CopyTo (
        new Span<byte>(filteredScanlinesBuffer.[0], 1, scanlineLength))
    scanline.CopyTo (
        new Span<byte>(filteredScanlinesBuffer.[2], 1, scanlineLength))

    // Also calculate the min sum of absolute diffs for those two filters
    sumsOfAbsDiffs.[0] <-
        filteredScanlinesBuffer.[0] |> Array.sumBy (fun x -> int x)
    sumsOfAbsDiffs.[2] <-
        filteredScanlinesBuffer.[2] |> Array.sumBy (fun x -> int x)

    for scanlineByteIndex in 0 .. (scanline.Length - 1) do
        let raw = scanline.[scanlineByteIndex]

        let mutable left = 0uy

        if scanlineByteIndex >= bytesPP then
            left <- scanline.[scanlineByteIndex-bytesPP]

        let filteredScanlineIndex = scanlineByteIndex + 1
        let mutable filteredValue = 0uy

        filteredValue <- filterTypeSub raw left 0 0
        filteredScanlinesBuffer.[1].[filteredScanlineIndex] <- filteredValue
        sumsOfAbsDiffs.[1] <- sumsOfAbsDiffs.[1] + (int)filteredValue;

        filteredValue <- filterTypeAverageFirstLine raw left
        filteredScanlinesBuffer.[3].[filteredScanlineIndex] <- filteredValue
        sumsOfAbsDiffs.[3] <- sumsOfAbsDiffs.[3] + (int)filteredValue;

        filteredValue <- filterTypePaethFirstLine raw left
        filteredScanlinesBuffer.[4].[filteredScanlineIndex] <- filteredValue
        sumsOfAbsDiffs.[4] <- sumsOfAbsDiffs.[4] + (int)filteredValue;

    filteredScanlineWithMinSumOfAbsDiffs 
        filteredScanlinesBuffer
        sumsOfAbsDiffs


let filterNonFirstScanline
    imageData
    scanlineIndex
    (bytesPP: int) 
    scanlineLength 
    (filteredScanlinesBuffer: FilteredScanline[]) =
    // there are five types of PNG adaptive filters
    let filterTypesCount = 5

    let prevScanline = 
        scanlineFromImageData 
                imageData scanlineLength (scanlineIndex-1)

    let scanline = 
        scanlineFromImageData imageData scanlineLength scanlineIndex

    let sumsOfAbsDiffs: int[] = Array.zeroCreate filterTypesCount

    // For None, simply copy the original scanline bytes unfiltered.
    scanline.CopyTo (
        new Span<byte>(filteredScanlinesBuffer.[0], 1, scanlineLength))

    // Also calculate the min sum of absolute diffs for None filter type.
    sumsOfAbsDiffs.[0] <-
        filteredScanlinesBuffer.[0] |> Array.sumBy (fun x -> int x)

    for scanlineByteIndex in 0 .. (scanline.Length - 1) do
        let raw = scanline.[scanlineByteIndex]

        let mutable left = 0uy
        let up = prevScanline.[scanlineByteIndex]
        let mutable upLeft = 0uy 

        if scanlineByteIndex >= bytesPP then
            left <- scanline.[scanlineByteIndex-bytesPP]
            upLeft <- prevScanline.[scanlineByteIndex-bytesPP]

        let filteredScanlineIndex = scanlineByteIndex + 1
        let mutable filteredValue = 0uy

        filteredValue <- filterTypeSub raw left 0 0
        filteredScanlinesBuffer.[1].[filteredScanlineIndex] <- filteredValue
        sumsOfAbsDiffs.[1] <- sumsOfAbsDiffs.[1] + (int)filteredValue;

        filteredValue <- filterTypeUp raw left up upLeft
        filteredScanlinesBuffer.[2].[filteredScanlineIndex] <- filteredValue
        sumsOfAbsDiffs.[2] <- sumsOfAbsDiffs.[2] + (int)filteredValue;

        filteredValue <- filterTypeAverage raw left up upLeft
        filteredScanlinesBuffer.[3].[filteredScanlineIndex] <- filteredValue
        sumsOfAbsDiffs.[3] <- sumsOfAbsDiffs.[3] + (int)filteredValue;

        filteredValue <- filterTypePaeth raw left up upLeft
        filteredScanlinesBuffer.[4].[filteredScanlineIndex] <- filteredValue
        sumsOfAbsDiffs.[4] <- sumsOfAbsDiffs.[4] + (int)filteredValue;

    filteredScanlineWithMinSumOfAbsDiffs 
        filteredScanlinesBuffer
        sumsOfAbsDiffs


/// <summary>
/// Filters the provided sequence of scanlines according to the PNG filtering 
/// mechanism.
/// </summary>
/// <param name="scanlines">An array of scanlines.</param>
/// <returns>
/// An array of filtered scanlines. Each filtered scanline corresponds to an
/// original scanline.
/// </returns>
let filterScanlines 
    imageWidth
    imageHeight
    (bpp: int)
    (imageData: ImageData)
    : FilteredImageData =

    let bytesPP = bytesPerPixel bpp
    let scanlineLength = imageWidth * bytesPP
    let filteredScanlineLength = scanlineLength + 1

    let filteredImageData: FilteredImageData = 
        Array.zeroCreate (filteredScanlineLength * imageHeight)

    let filterScanline scanlineIndex filteredScanlinesBuffer: unit =
        let filteredScanline = 
            match scanlineIndex with
            | 0 -> 
                filterFirstScanline
                    imageData 
                    bytesPP 
                    scanlineLength 
                    filteredScanlinesBuffer
            | _ -> 
                filterNonFirstScanline
                    imageData 
                    scanlineIndex 
                    bytesPP 
                    scanlineLength 
                    filteredScanlinesBuffer

        // Copy the chosen filtered scanline into the big array.
        let filteredScanlineImageDataIndex = 
            scanlineIndex * filteredScanlineLength
        Array.blit 
            filteredScanline 
            0 
            filteredImageData 
            filteredScanlineImageDataIndex
            filteredScanlineLength

    let forInit() = createFilteredScanlinesBuffer filteredScanlineLength
    let forBody scanlineIndex _ filteredScanlinesBuffer = 
        filterScanline scanlineIndex filteredScanlinesBuffer
        filteredScanlinesBuffer

    Parallel.For(
        0, 
        imageHeight, 
        forInit,
        forBody,
        fun (_) -> ignore())
    |> ignore

    filteredImageData
