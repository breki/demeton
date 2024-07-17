module Png.Unfilters

open Raster
open Png.Filters

open System


let inline unfilterScanlineNone (filtered: Span<byte>) (scanline: Span<byte>) =
    filtered.CopyTo(scanline)


/// <summary>
/// Converts a filtered scanline back to the original scanline using the "Sub"
/// PNG filter method.
/// </summary>
let unfilterScanlineSub
    bytesPP
    (filtered: Span<byte>)
    (scanline: Span<byte>)
    : unit =
    let scanlineLength = scanline.Length

    // how many pixels are there in the scanline?
    let pixelCount = scanlineLength / bytesPP
    let pixelCountMinus1 = pixelCount - 1

    // for each byte in pixel
    for pixelByteOffset in 0 .. (bytesPP - 1) do
        // keep track of the left neighbor value
        let mutable leftValue = filtered.[pixelByteOffset]

        // for the first (leftmost) pixel, we cannot subtract the value
        // since there is no left neighbor, so we just copy the value from
        // the filtered scanline
        scanline.[pixelByteOffset] <- leftValue

        // for the rest of the pixels
        for i in 1..pixelCountMinus1 do
            // calculate its index in the scanline
            let byteIndex = pixelByteOffset + i * bytesPP

            let value = leftValue + filtered.[byteIndex]
            scanline.[byteIndex] <- value
            leftValue <- value


/// <summary>
/// Converts a filtered scanline back to the original scanline using the "Up"
/// PNG filter method.
/// </summary>
let unfilterScanlineUp
    _
    (prevScanline: Span<byte>)
    (filtered: Span<byte>)
    (scanline: Span<byte>)
    : unit =
    let scanlineLength = filtered.Length

    match prevScanline.Length with
    | 0 -> filtered.CopyTo(scanline)
    | _ ->
        for i in 0 .. (scanlineLength - 1) do
            scanline.[i] <- filtered.[i] + prevScanline.[i]


/// <summary>
/// Convert a filtered scanline back to the original scanline using the
/// "Average" PNG filter method.
/// </summary>
let unfilterScanlineAverage
    bytesPP
    (prevScanline: Span<byte>)
    (filtered: Span<byte>)
    (scanline: Span<byte>)
    : unit =

    let scanlineLength = filtered.Length

    // how many pixels are there in the scanline?
    let pixelCount = scanlineLength / bytesPP
    let pixelCountMinus1 = pixelCount - 1

    // for each byte in pixel
    for pixelByteOffset in 0 .. (bytesPP - 1) do

        // keep track of the left (previous) neighbor value
        let mutable left = 0

        // for each pixel
        for i in 0..pixelCountMinus1 do
            // calculate its index in the scanline
            let byteIndex = pixelByteOffset + i * bytesPP

            let currentValue = filtered.[byteIndex]

            let up =
                match prevScanline.Length with
                | 0 -> 0
                | _ -> int prevScanline.[byteIndex]

            let unfilteredValue = currentValue + byte ((left + up) / 2)
            left <- int unfilteredValue
            scanline.[byteIndex] <- unfilteredValue


/// <summary>
/// Convert a filtered scanline back to the original scanline using the
/// "Paeth" PNG filter method.
/// </summary>
let unfilterScanlinePaeth
    bytesPP
    (prevScanline: Span<byte>)
    (filtered: Span<byte>)
    (scanline: Span<byte>)
    : unit =

    let scanlineLength = filtered.Length

    // how many pixels are there in the scanline?
    let pixelCount = scanlineLength / bytesPP
    let pixelCountMinus1 = pixelCount - 1

    // for each byte in pixel
    for pixelByteOffset in 0 .. (bytesPP - 1) do

        // keep track of the left (previous) neighbor value
        let mutable left = 0

        // for each pixel
        for i in 0..pixelCountMinus1 do
            // calculate its index in the scanline
            let byteIndex = pixelByteOffset + i * bytesPP

            let currentValue = filtered.[byteIndex]

            let up =
                match prevScanline.Length with
                | 0 -> 0
                | _ -> int prevScanline.[byteIndex]

            let upLeft =
                match (i, prevScanline.Length) with
                | 0, _ -> 0
                | _, 0 -> 0
                | _, _ -> int prevScanline.[byteIndex - bytesPP]

            let unfilteredValue =
                currentValue + byte (paethPredictor left up upLeft)

            left <- int unfilteredValue
            scanline.[byteIndex] <- unfilteredValue


let unfilterScanline
    bytesPP
    filterType
    (prevScanline: Span<byte>)
    (filteredScanline: Span<byte>)
    (scanline: Span<byte>)
    : unit =

    match filterType with
    | 0 -> unfilterScanlineNone filteredScanline scanline
    | 1 -> unfilterScanlineSub bytesPP filteredScanline scanline
    | 2 -> unfilterScanlineUp bytesPP prevScanline filteredScanline scanline
    | 3 ->
        unfilterScanlineAverage bytesPP prevScanline filteredScanline scanline
    | 4 -> unfilterScanlinePaeth bytesPP prevScanline filteredScanline scanline
    | _ -> invalidOp (sprintf "Unsupported PNG filter type %d" filterType)


let unfilterScanlines
    imageWidth
    imageHeight
    bpp
    (filteredImageData: byte[])
    : RawImageData =

    let bytesPP = bytesPerPixel bpp
    let scanlineLength = imageWidth * bytesPP

    let imageData: RawImageData =
        Array.zeroCreate (scanlineLength * imageHeight)

    let mutable prevScanline = Span<byte>()

    for scanlineIndex in 0 .. (imageHeight - 1) do
        let filteredScanlineFirstByteIndex =
            scanlineIndex * (scanlineLength + 1)

        let filterType = int filteredImageData.[filteredScanlineFirstByteIndex]

        let filteredScanline =
            Span<byte>(
                filteredImageData,
                filteredScanlineFirstByteIndex + 1,
                scanlineLength
            )

        let scanline =
            Span<byte>(
                imageData,
                scanlineIndex * scanlineLength,
                scanlineLength
            )

        unfilterScanline
            bytesPP
            filterType
            prevScanline
            filteredScanline
            scanline

        prevScanline <- scanline

    imageData
