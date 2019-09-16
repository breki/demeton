module Demeton.PngFilters

open Demeton.PngTypes

open System

let bytesPerPixel bpp =
    let bytes = bpp / 8
    let bitRemainder = bpp % 8
    match (bytes, bitRemainder) with
    | (_, 0) -> bytes
    | (0, _) -> 
        invalidOp "Bit depths less than 8 bits are currently not supported."
    | (_, _) -> 
        invalidOp "Invalid bpp (bits-per-pixel) value."


let inline unfilterScanlineNone 
        (filtered: Span<byte>) (scanline: Span<byte>) =
    filtered.CopyTo(scanline)


/// <summary>
/// Converts a filtered scaline back to the original scanline using the "Sub"
/// PNG filter method.
/// </summary>
/// <param name="bpp">Bits per pixel of the image.</param>
/// <param name="filtered">
/// The filtered scanline that should be converted.
/// </param>
/// <returns>The original (non-filtered) scanline.</returns>
let unfilterScanlineSub 
    bytesPP (filtered: Span<byte>) (scanline: Span<byte>): unit =
    let scanlineLength = scanline.Length

    match (scanlineLength, scanlineLength % bytesPP) with
    | (0, _) -> ignore()
    | (_, 0) -> 
        // how many pixels are there in the scanline?
        let pixelCount = scanlineLength / bytesPP

        // for each byte in pixel
        for pixelByteOffset in 0 .. (bytesPP-1) do

            // keep track of the left neighbor value
            let mutable leftValue = filtered.[pixelByteOffset]

            // for the first (leftmost) pixel, we cannot subtract the value 
            // since there is no left neighbor, so we just copy the value from
            // the filtered scanline
            scanline.[pixelByteOffset] <- leftValue

            // for the rest of the pixels
            for i in 1 .. pixelCount-1 do
                // calculate its index in the scanline
                let byteIndex = pixelByteOffset + i * bytesPP

                let value = leftValue + filtered.[byteIndex] 
                scanline.[byteIndex] <- value
                leftValue <- value

    | (_, _) -> invalidOp "Invalid scanline length"


/// <summary>
/// Converts a filtered scaline back to the original scanline using the "Up"
/// PNG filter method.
/// </summary>
/// <param name="prevScanline">
/// The previous (upper) scanline that is used to calculate filter values.
/// </param>
/// <param name="filtered">
/// The filtered scanline that should be converted.
/// </param>
/// <returns>The original (non-filtered) scanline.</returns>
let unfilterScanlineUp 
    _
    (prevScanline: Span<byte>) 
    (filtered: Span<byte>)
    (scanline: Span<byte>)
    : unit =
    let scanlineLength = filtered.Length

    match scanlineLength with
    | l when (l < 0) -> 
        invalidArg "filtered byte array cannot be empty" "filtered"
    | 0 -> ignore()
    | _ -> 
        match prevScanline.Length with
        | 0 -> filtered.CopyTo(scanline)
        | _ -> 
            for i in 0 .. (scanlineLength-1) do
                scanline.[i] <- filtered.[i] + prevScanline.[i]


/// <summary>
/// Converts a filtered scaline back to the original scanline using the 
/// "Average" PNG filter method.
/// </summary>
/// <param name="bpp">Bits per pixel of the image.</param>
/// <param name="prevScanline">
/// The previous (upper) scanline that is used to calculate filter values.
/// </param>
/// <param name="filtered">
/// The filtered scanline that should be converted.
/// </param>
/// <returns>The original (non-filtered) scanline.</returns>
let unfilterScanlineAverage 
    bytesPP
    (prevScanline: Span<byte>) 
    (filtered: Span<byte>)
    (scanline: Span<byte>)
    : unit =

    let scanlineLength = filtered.Length

    match (scanlineLength, scanlineLength % bytesPP) with
    | (0, _) -> ignore()
    | (_, 0) -> 
        // how many pixels are there in the scanline?
        let pixelCount = scanlineLength / bytesPP

        // for each byte in pixel
        for pixelByteOffset in 0 .. (bytesPP-1) do

            // keep track of the left (previous) neighbor value
            let mutable left = 0
        
            // for each pixel
            for i in 0 .. pixelCount-1 do
                // calculate its index in the scanline
                let byteIndex = pixelByteOffset + i * bytesPP

                let currentValue = filtered.[byteIndex]
                let up =
                    match prevScanline.Length with
                    | 0 -> 0
                    | _ -> (int)prevScanline.[byteIndex]

                let unfilteredValue = 
                    currentValue + (byte)((left + up) / 2)
                left <- (int) unfilteredValue
                scanline.[byteIndex] <- unfilteredValue

    | (_, _) -> invalidOp "Invalid scanline length"


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
let unfilterScanlinePaeth 
    bytesPP
    (prevScanline: Span<byte>) 
    (filtered: Span<byte>)
    (scanline: Span<byte>)
    : unit =

    let scanlineLength = filtered.Length

    match (scanlineLength, scanlineLength % bytesPP) with
    | (0, _) -> ignore()
    | (_, 0) -> 
        // how many pixels are there in the scanline?
        let pixelCount = scanlineLength / bytesPP

        // for each byte in pixel
        for pixelByteOffset in 0 .. (bytesPP-1) do

            // keep track of the left (previous) neighbor value
            let mutable left = 0

            // for each pixel
            for i in 0 .. pixelCount-1 do
                // calculate its index in the scanline
                let byteIndex = pixelByteOffset + i * bytesPP

                let currentValue = filtered.[byteIndex]
                let up =
                    match prevScanline.Length with
                    | 0 -> 0
                    | _ -> (int)prevScanline.[byteIndex]
                let upLeft = 
                    match (i, prevScanline.Length) with
                    | (0, _) -> 0
                    | (_, 0) -> 0
                    | (_, _) -> (int)prevScanline.[byteIndex-bytesPP]

                let unfilteredValue = 
                    currentValue + (byte)(paethPredictor left up upLeft)
                left <- (int)unfilteredValue
                scanline.[byteIndex] <- unfilteredValue

    | (_, _) -> invalidOp "Invalid scanline length"


let inline filterTypeNone raw = raw

let inline filterTypeSub raw left _ _ = raw - left

let inline filterTypeUp raw _ up _ = raw - up

let inline filterTypeAverage raw (left: byte) (up: byte) _ 
    = raw - (byte)(((int)left + (int)up) / 2)

let inline filterTypePaeth raw (left: byte) (up: byte) (upLeft: byte) = 
    raw - (byte)(paethPredictor ((int)left) ((int)up) ((int)upLeft))


type ScanlineFilterMultiple = 
    int -> Scanline option -> Scanline -> FilteredScanline[] 
        -> FilteredScanline


let filterScanline
    (bytesPP: int) 
    (prevScanline: Scanline option) 
    (scanline: Scanline) 
    (filteredScanlinesBuffer: FilteredScanline[]) =

    // there are five types of PNG adaptive filters
    let filtersCount = 5

    let sumsOfAbsDiffs: int[] = Array.zeroCreate filtersCount

    for scanlineIndex in 0 .. (scanline.Length - 1) do
        let raw = scanline.[scanlineIndex]

        let mutable left = 0uy
        let mutable up = 0uy
        let mutable upLeft = 0uy

        if scanlineIndex >= bytesPP then
            left <- scanline.[scanlineIndex-bytesPP]

        if Option.isSome prevScanline then
            let prevScanlineVal = prevScanline.Value
            up <- prevScanlineVal.[scanlineIndex]
            if scanlineIndex >= bytesPP then
                upLeft <- prevScanlineVal.[scanlineIndex-bytesPP]

        let filteredScanlineIndex = scanlineIndex + 1
        let mutable filteredValue = 0uy

        // this is a shortcut for filter type None
        filteredValue <- raw
        filteredScanlinesBuffer.[0].[filteredScanlineIndex] <- filteredValue
        sumsOfAbsDiffs.[0] <- sumsOfAbsDiffs.[0] + (int)filteredValue;

        filteredValue <- filterTypeSub raw left up upLeft
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

    let filteredScanlineWithMinSumOfAbsDiffs() =
        let mutable minIndex = -1
        let mutable minValueSoFar = System.Int32.MaxValue

        for i in 0 .. (filtersCount - 1) do
            let minValueOfFilter = sumsOfAbsDiffs.[i]
            if minValueOfFilter < minValueSoFar then
                minIndex <- i
                minValueSoFar <- minValueOfFilter
            else ignore()

        filteredScanlinesBuffer.[minIndex]

    filteredScanlineWithMinSumOfAbsDiffs()


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
    (filter: ScanlineFilterMultiple)
    imageWidth
    imageHeight
    (bpp: int)
    (imageData: ImageData)
    : FilteredScanline[]=

    // there are five types of PNG adaptive filters
    let filterTypesCount = 5

    let bytesPP = bytesPerPixel bpp
    let scanlineLength = imageWidth * bytesPP
    let filteredScanlineLength = scanlineLength + 1

    let scanlineFromImageData index =
        Array.sub imageData (index * scanlineLength) scanlineLength

    let filterScanline scanlineIndex =
        let filteredScanlinesBuffer: FilteredScanline[] = 
            Array.init 
                filterTypesCount 
                (fun i -> Array.zeroCreate filteredScanlineLength)

        let addFilterTypeByteMarks() = 
            for filterIndex in 0 .. (filterTypesCount-1) do
                filteredScanlinesBuffer.[filterIndex].[0] <- (byte)filterIndex

        addFilterTypeByteMarks()

        let prevScanline =
            match scanlineIndex with
            | 0 -> None
            | _ -> Some (scanlineFromImageData (scanlineIndex - 1))

        let scanline = scanlineFromImageData(scanlineIndex)
        let filteredScanline = 
            filter 
                bytesPP prevScanline scanline filteredScanlinesBuffer
        // We have to make a clone of the filtered scanline as the one we
        // received is reused (as part of the buffer) on each scanline.
        Array.copy filteredScanline

    Array.Parallel.init imageHeight filterScanline


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
    | 3 -> unfilterScanlineAverage bytesPP prevScanline filteredScanline scanline
    | 4 -> unfilterScanlinePaeth bytesPP prevScanline filteredScanline scanline
    | _ -> 
        invalidOp (sprintf "Unsupported PNG filter type %d" filterType)


let unfilterScanlines 
    imageWidth imageHeight bpp (filteredImageData: byte[]): ImageData =

    let bytesPP = bytesPerPixel bpp
    let scanlineLength = imageWidth * bytesPP

    let imageData: ImageData = 
        Array.zeroCreate (scanlineLength * imageHeight)

    let mutable prevScanline = new Span<byte>()
    
    for scanlineIndex in 0 .. (imageHeight-1) do
        let filteredScanlineFirstByteIndex = 
            scanlineIndex * (scanlineLength + 1)
        let filterType = int filteredImageData.[filteredScanlineFirstByteIndex]

        let filteredScanline = 
            new Span<byte>(
                filteredImageData, 
                filteredScanlineFirstByteIndex + 1,
                scanlineLength)
        let scanline = 
            new Span<byte>(
                imageData,
                scanlineIndex * scanlineLength,
                scanlineLength)

        unfilterScanline 
            bytesPP filterType prevScanline filteredScanline scanline
        prevScanline <- scanline

    imageData
