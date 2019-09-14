module Demeton.PngFilters

open Demeton.PngTypes


let bytesPerPixel bpp =
    let bytes = bpp / 8
    let bitRemainder = bpp % 8
    match (bytes, bitRemainder) with
    | (_, 0) -> bytes
    | (0, _) -> 
        invalidOp "Bit depths less than 8 bits are currently not supported."
    | (_, _) -> 
        invalidOp "Invalid bpp (bits-per-pixel) value."


let filterScanline 
    (filterValue: FilterTypeFunc)
    (filterType: byte)
    (bytesPP: int)
    (prevScanline: Scanline option) 
    (scanline: Scanline)
    : (FilteredScanline * int) =

    let mutable sumOfAbsDifferences = 0

    let inline fillFilteredScanlineValue (filteredScanlineIndex: int) =
        let filteredValue = 
            match filteredScanlineIndex with
            | 0 -> filterType
            | _ -> 
                filterValue 
                    (filteredScanlineIndex-1) bytesPP prevScanline scanline
        sumOfAbsDifferences <- sumOfAbsDifferences + (int) filteredValue
        filteredValue

    (Array.init (scanline.Length + 1) fillFilteredScanlineValue, 
        sumOfAbsDifferences)
    

let inline filterTypeNone scanlineIndex _ _ (scanline: Scanline): byte = 
    scanline.[scanlineIndex]

let inline unfilterScanlineNone _ _ (filtered: FilteredScanline) =
    [| for i in 0 .. filtered.Length - 2 -> filtered.[i + 1] |]


/// <summary>
/// Filters the specified scaline using the "Sub" PNG filter method.
/// </summary>
/// <param name="bpp">Bits per pixel of the image.</param>
/// <param name="scanline">The scanline that should be filtered.</param>
/// <returns>The filtered scanline.</returns>
let inline filterTypeSub scanlineIndex bytesPP _ (scanline: Scanline): byte = 
    match scanlineIndex with
    | x when x < bytesPP -> scanline.[scanlineIndex]
    | x -> 
        scanline.[scanlineIndex] - scanline.[scanlineIndex-bytesPP]

/// <summary>
/// Converts a filtered scaline back to the original scanline using the "Sub"
/// PNG filter method.
/// </summary>
/// <param name="bpp">Bits per pixel of the image.</param>
/// <param name="filtered">
/// The filtered scanline that should be converted.
/// </param>
/// <returns>The original (non-filtered) scanline.</returns>
let unfilterScanlineSub bytesPP _ (filtered: FilteredScanline): Scanline =
    let scanlineLength = filtered.Length - 1

    match (scanlineLength, scanlineLength % bytesPP) with
    | (0, _) -> [||]
    | (_, 0) -> 
        let scanline = Array.zeroCreate scanlineLength

        // how many pixels are there in the scanline?
        let pixelCount = scanlineLength / bytesPP

        // for each byte in pixel
        for pixelByteOffset in 0 .. (bytesPP-1) do

            // keep track of the left neighbor value
            let mutable leftValue = filtered.[1 + pixelByteOffset]

            // for the first (leftmost) pixel, we cannot subtract the value 
            // since there is no left neighbor, so we just copy the value from
            // the filtered scanline
            scanline.[pixelByteOffset] <- leftValue

            // for the rest of the pixels
            for i in 1 .. pixelCount-1 do
                // calculate its index in the scanline
                let byteIndex = pixelByteOffset + i * bytesPP

                let value = leftValue + filtered.[1 + byteIndex] 
                scanline.[byteIndex] <- value
                leftValue <- value

        scanline
    | (_, _) -> invalidOp "Invalid scanline length"


/// <summary>
/// Filters the specified scaline using the "Up" PNG filter method.
/// </summary>
/// <param name="prevScanline">
/// The previous (upper) scanline that is used to calculate filter values.
/// </param>
/// <param name="scanline">The scanline that should be filtered.</param>
/// <returns>The filtered scanline.</returns>
let inline filterTypeUp 
    scanlineIndex
    _
    (prevScanline: Scanline option) 
    (scanline: Scanline)
    : byte = 

    let up = 
        match prevScanline with
        | None -> 0uy
        | Some prevScanline -> prevScanline.[scanlineIndex]
    scanline.[scanlineIndex] - up


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
    (prevScanline: Scanline option) (filtered: FilteredScanline): Scanline =
    let scanlineLength = filtered.Length - 1

    match scanlineLength with
    | l when (l < 0) -> 
        invalidArg "filtered byte array cannot be empty" "filtered"
    | 0 -> [||]
    | _ -> 
        match prevScanline with
        | None -> filtered |> Array.skip 1
        | Some prev -> 
            let scanline = Array.zeroCreate scanlineLength

            for i in 1 .. scanlineLength do
                scanline.[i-1] <- filtered.[i] + prev.[i-1]

            scanline


/// <summary>
/// Filters the specified scaline using the "Average" PNG filter method.
/// </summary>
/// <param name="bpp">Bits per pixel of the image.</param>
/// <param name="prevScanline">
/// The previous (upper) scanline that is used to calculate filter values.
/// </param>
/// <param name="scanline">The scanline that should be filtered.</param>
/// <returns>The filtered scanline.</returns>
let inline filterTypeAverage 
    scanlineIndex bytesPP (prevScanline: Scanline option) (scanline: Scanline)
    : byte = 

    let raw = scanline.[scanlineIndex]
    let left = 
        match scanlineIndex with
        | x when x < bytesPP -> 0
        | _ -> (int)scanline.[scanlineIndex-bytesPP]
    let up = 
        match prevScanline with
        | None -> 0
        | Some prev -> (int)prev.[scanlineIndex]
    raw - (byte)((left + up) / 2)


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
    (prevScanline: Scanline option) (filtered: FilteredScanline): Scanline =

    let scanlineLength = filtered.Length - 1

    match (scanlineLength, scanlineLength % bytesPP) with
    | (0, _) -> [||]
    | (_, 0) -> 
        let scanline = Array.zeroCreate scanlineLength

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

                let currentValue = filtered.[1 + byteIndex]
                let up =
                    match prevScanline with
                    | None -> 0
                    | Some prev -> (int)prev.[byteIndex]

                let unfilteredValue = 
                    currentValue + (byte)((left + up) / 2)
                left <- (int) unfilteredValue
                scanline.[byteIndex] <- unfilteredValue

        scanline
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
/// Filters the specified scaline using the "Paeth" PNG filter method.
/// </summary>
/// <param name="bpp">Bits per pixel of the image.</param>
/// <param name="prevScanline">
/// The previous (upper) scanline that is used to calculate filter values.
/// </param>
/// <param name="scanline">The scanline that should be filtered.</param>
/// <returns>The filtered scanline.</returns>
let inline filterTypePaeth 
    scanlineIndex bytesPP (prevScanline: Scanline option) (scanline: Scanline)
    : byte = 

    let raw = scanline.[scanlineIndex]
    let left = 
        match scanlineIndex with
        | x when x < bytesPP -> 0
        | _ -> (int)scanline.[scanlineIndex-bytesPP]
    let up = 
        match prevScanline with
        | None -> 0
        | Some prev -> (int) prev.[scanlineIndex]
    let upLeft = 
        match (scanlineIndex, prevScanline) with
        | (x, _) when x < bytesPP -> 0
        | (_, None) -> 0
        | (_, Some prev) -> (int) prev.[scanlineIndex-bytesPP]
    raw - (byte)(paethPredictor left up upLeft)


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
    (prevScanline: Scanline option) (filtered: FilteredScanline): Scanline =

    let scanlineLength = filtered.Length - 1

    match (scanlineLength, scanlineLength % bytesPP) with
    | (0, _) -> [||]
    | (_, 0) -> 
        let scanline = Array.zeroCreate scanlineLength

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

                let currentValue = filtered.[1 + byteIndex]
                let up =
                    match prevScanline with
                    | None -> 0
                    | Some prev -> (int)prev.[byteIndex]
                let upLeft = 
                    match (i, prevScanline) with
                    | (0, _) -> 0
                    | (_, None) -> 0
                    | (_, Some prev) -> (int)prev.[byteIndex-bytesPP]

                let unfilteredValue = 
                    currentValue + (byte)(paethPredictor left up upLeft)
                left <- (int)unfilteredValue
                scanline.[byteIndex] <- unfilteredValue

        scanline
    | (_, _) -> invalidOp "Invalid scanline length"


let allPngFilters: FilterTypeFunc[] = [|
    filterTypeNone; filterTypeSub; filterTypeUp; 
    filterTypeAverage; filterTypePaeth
|]


let allPngUnfilters: ScanlineUnfilter[] = [|
    unfilterScanlineNone; unfilterScanlineSub; unfilterScanlineUp; 
    unfilterScanlineAverage; unfilterScanlinePaeth
|]


let minSumOfAbsoluteDifferencesSelector
    bytesPP
    (prevScanline: Scanline option) 
    (scanline: Scanline)
    : (FilteredScanline * int) =

    allPngFilters 
    |> Array.mapi (
        fun filterType filterValueFunc -> 
                filterScanline 
                    filterValueFunc ((byte)filterType) bytesPP prevScanline scanline)
    |> Array.minBy (fun (_, sum) -> sum)


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
    (filter: ScanlineFilter)
    (bpp: int)
    (scanlines: Scanline[])
    : FilteredScanline[]=

    let bytesPP = bytesPerPixel bpp

    [| 
        for scanlineIndex in 0 .. scanlines.Length - 1 do
            let prevScanline =
                match scanlineIndex with
                | 0 -> None
                | _ -> Some scanlines.[scanlineIndex - 1]

            let scanline = scanlines.[scanlineIndex]
            let (filteredScanline, _) = filter bytesPP prevScanline scanline
            yield filteredScanline
    |]

type ScanlineFilter2 = byte -> byte -> byte -> byte -> byte

let inline filterTypeNone2 raw _ _ _ = raw

let inline filterTypeSub2 raw left _ _ = raw - left

let inline filterTypeUp2 raw _ up _ = raw - up

let inline filterTypeAverage2 raw (left: byte) (up: byte) _ 
    = raw - (byte)(((int)left + (int)up) / 2)

let inline filterTypePaeth2 raw (left: byte) (up: byte) (upLeft: byte) = 
    raw - (byte)(paethPredictor ((int)left) ((int)up) ((int)upLeft))

let allPngFilters2: ScanlineFilter2[] = 
    [| filterTypeNone2; filterTypeSub2; filterTypeUp2; filterTypeAverage2;
        filterTypePaeth2 |]

type ScanlineFilterMultiple = 
    int -> Scanline option -> Scanline -> FilteredScanline[] 
        -> FilteredScanline

let scanlineFilterMultiple
    (bytesPP: int) 
    (prevScanline: Scanline option) 
    (scanline: Scanline) 
    (filteredScanlinesBuffer: FilteredScanline[]) =

    let filtersCount = allPngFilters2.Length

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

        filteredValue <- filterTypeNone2 raw left up upLeft
        filteredScanlinesBuffer.[0].[filteredScanlineIndex] <- filteredValue
        sumsOfAbsDiffs.[0] <- sumsOfAbsDiffs.[0] + (int)filteredValue;

        filteredValue <- filterTypeSub2 raw left up upLeft
        filteredScanlinesBuffer.[1].[filteredScanlineIndex] <- filteredValue
        sumsOfAbsDiffs.[1] <- sumsOfAbsDiffs.[1] + (int)filteredValue;

        filteredValue <- filterTypeUp2 raw left up upLeft
        filteredScanlinesBuffer.[2].[filteredScanlineIndex] <- filteredValue
        sumsOfAbsDiffs.[2] <- sumsOfAbsDiffs.[2] + (int)filteredValue;

        filteredValue <- filterTypeAverage2 raw left up upLeft
        filteredScanlinesBuffer.[3].[filteredScanlineIndex] <- filteredValue
        sumsOfAbsDiffs.[3] <- sumsOfAbsDiffs.[3] + (int)filteredValue;

        filteredValue <- filterTypePaeth2 raw left up upLeft
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

let filterScanlines2 
    (filter: ScanlineFilterMultiple)
    (bpp: int)
    (scanlines: Scanline[])
    : FilteredScanline[]=

    let filterTypesCount = allPngFilters2.Length

    let filteredScanlineLength = scanlines.[0].Length + 1
    let filteredScanlinesBuffer: FilteredScanline[] = 
        Array.init 
            filterTypesCount 
            (fun i -> Array.zeroCreate filteredScanlineLength)

    let addFilterTypeByteMarks() = 
        for filterIndex in 0 .. (filterTypesCount-1) do
            filteredScanlinesBuffer.[filterIndex].[0] <- (byte)filterIndex

    addFilterTypeByteMarks()

    let bytesPP = bytesPerPixel bpp

    let filterScanline scanlineIndex =
        let prevScanline =
            match scanlineIndex with
            | 0 -> None
            | _ -> Some scanlines.[scanlineIndex - 1]

        let scanline = scanlines.[scanlineIndex]
        let filteredScanline = 
            filter 
                bytesPP prevScanline scanline filteredScanlinesBuffer
        // We have to make a clone of the filtered scanline as the one we
        // received is reused (as part of the buffer) on each scanline.
        Array.copy filteredScanline

    Array.init scanlines.Length filterScanline


let unfilterScanlines bpp (filteredScanlines: FilteredScanline[]): Scanline[] =
    let bytesPP = bytesPerPixel bpp

    [| 
        let mutable prevScanline = None
        for scanlineIndex in 0 .. filteredScanlines.Length - 1 do
            let filteredScanline = filteredScanlines.[scanlineIndex]

            let filterType = (int)filteredScanline.[0]
            match filterType with
            | f when f >= 0 && f < allPngUnfilters.Length -> 
                let unfilteredScanline = 
                    allPngUnfilters.[filterType] 
                        bytesPP prevScanline filteredScanline
                prevScanline <- Some unfilteredScanline
                yield unfilteredScanline
            | _ -> 
                invalidOp (sprintf "Unsupported PNG filter type %d" filterType)
    |]
