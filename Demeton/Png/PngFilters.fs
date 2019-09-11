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


let filterScanlineNone _ _ (scanline: Scanline): FilteredScanline =    
        [| 
            for i in 0 .. scanline.Length -> 
                match i with
                | 0 -> (byte)FilterType.FilterNone
                | x -> scanline.[x - 1]
        |]


let unfilterScanlineNone _ _ (filtered: FilteredScanline) =
    [| for i in 0 .. filtered.Length - 2 -> filtered.[i + 1] |]


/// <summary>
/// Filters the specified scaline using the "Sub" PNG filter method.
/// </summary>
/// <param name="bpp">Bits per pixel of the image.</param>
/// <param name="scanline">The scanline that should be filtered.</param>
/// <returns>The filtered scanline.</returns>
let filterScanlineSub bpp _ (scanline: Scanline): FilteredScanline = 
    let bytesPP = bytesPerPixel bpp

    [| 
        for filteredIndex in 0 .. scanline.Length -> 
            let scanlineIndex = filteredIndex - 1

            match scanlineIndex with
            | -1 -> (byte)FilterType.FilterSub
            | x when x < bytesPP -> scanline.[x]
            | x -> 
                scanline.[x] - scanline.[x-bytesPP]
    |]


/// <summary>
/// Converts a filtered scaline back to the original scanline using the "Sub"
/// PNG filter method.
/// </summary>
/// <param name="bpp">Bits per pixel of the image.</param>
/// <param name="filtered">
/// The filtered scanline that should be converted.
/// </param>
/// <returns>The original (non-filtered) scanline.</returns>
let unfilterScanlineSub bpp _ (filtered: FilteredScanline): Scanline =
    let scanlineLength = filtered.Length - 1
    let bytesPP = bytesPerPixel bpp

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
/// <param name="scanline">The scanline that should be filtered.</param>
/// <returns>The filtered scanline.</returns>
let filterScanlineUp 
    _
    (prevScanline: Scanline option) 
    (scanline: Scanline)
    : FilteredScanline = 

    match prevScanline with
    | None -> 
        [| 
            for i in 0 .. scanline.Length -> 
                match i with
                | 0 -> (byte)FilterType.FilterUp
                | x -> scanline.[x-1]
        |]
    | Some prev -> 
        let filtered =[| 
            for i in 0 .. scanline.Length -> 
                match i with
                | 0 -> (byte)FilterType.FilterUp
                | x -> scanline.[x-1] - prev.[x-1]
            |]
        filtered


/// <summary>
/// Converts a filtered scaline back to the original scanline using the "Up"
/// PNG filter method.
/// </summary>
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
/// <param name="scanline">The scanline that should be filtered.</param>
/// <returns>The filtered scanline.</returns>
let filterScanlineAverage 
    bpp (prevScanline: Scanline option) (scanline: Scanline): FilteredScanline = 
    let bytesPP = bytesPerPixel bpp

    [| 
        for filteredIndex in 0 .. scanline.Length -> 
            let scanlineIndex = filteredIndex - 1

            match scanlineIndex with
            | -1 -> (byte)FilterType.FilterAverage
            | _ -> 
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
    |]


/// <summary>
/// Converts a filtered scaline back to the original scanline using the 
/// "Average" PNG filter method.
/// </summary>
/// <param name="bpp">Bits per pixel of the image.</param>
/// <param name="filtered">
/// The filtered scanline that should be converted.
/// </param>
/// <returns>The original (non-filtered) scanline.</returns>
let unfilterScanlineAverage 
    bpp
    (prevScanline: Scanline option) (filtered: FilteredScanline): Scanline =

    let scanlineLength = filtered.Length - 1
    let bytesPP = bytesPerPixel bpp

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


let paethPredictor (a:int) (b:int) (c:int) = 
    let p = a + b - c
    let pa = abs (p - a)
    let pb = abs (p - b)
    let pc = abs (p - c)
    match (pa, pb, pc) with
    | (_, _, _) when pa <= pb && pa <= pc -> a
    | (_, _, _) when pb <= pc -> b
    | _ -> c


let filterScanlinePaeth 
    bpp
    (prevScanline: Scanline option) (scanline: Scanline): FilteredScanline = 
    [| 
        for i in 0 .. scanline.Length -> 
            match i with
            | 0 -> (byte)FilterType.FilterPaeth
            | x -> 
                let raw = scanline.[x-1]
                let left = 
                    match x with
                    | 1 -> 0
                    | _ -> (int)scanline.[x-2]
                let up = 
                    match prevScanline with
                    | None -> 0
                    | Some prev -> (int) prev.[x-1]
                let upLeft = 
                    match (x, prevScanline) with
                    | (1, _) -> 0
                    | (_, None) -> 0
                    | (_, Some prev) -> (int) prev.[x-2]
                raw 
                    - (byte)(paethPredictor left up upLeft)
    |]


let unfilterScanlinePaeth 
    bpp
    (prevScanline: Scanline option) (filtered: FilteredScanline): Scanline =
    [|
        let mutable lastRaw = filtered.[1]
        
        for i in 0 .. filtered.Length - 2 ->
            let filtered = filtered.[i + 1]
            let left = 
                match i with
                | 0 -> 0
                | _ -> (int)lastRaw
            let up =
                match prevScanline with
                | None -> 0
                | Some prev -> (int)prev.[i]
            let upLeft = 
                match (i, prevScanline) with
                | (0, _) -> 0
                | (_, None) -> 0
                | (_, Some prev) -> (int)prev.[i-1]

            let unfilteredValue = 
                filtered + (byte)(paethPredictor left up upLeft)
            lastRaw <- unfilteredValue
            unfilteredValue
    |]


let allPngFilters: ScanlineFilter[] = [|
    filterScanlineNone; filterScanlineSub; filterScanlineUp; 
    filterScanlineAverage; filterScanlinePaeth
|]


let allPngUnfilters: ScanlineUnfilter[] = [|
    unfilterScanlineNone; unfilterScanlineSub; unfilterScanlineUp; 
    unfilterScanlineAverage; unfilterScanlinePaeth
|]


let sumOfAbsoluteValueOfFilteredScanline (filtered: FilteredScanline): int =
    filtered |> Array.map int |> Array.sum


let minSumOfAbsoluteValueSelector
    bpp
    (prevScanline: Scanline option) 
    (scanline: Scanline)
    : FilteredScanline =

    let (filtered, _) = 
        allPngFilters 
        |> Seq.map (fun filter -> filter bpp prevScanline scanline)
        |> Seq.map (
            fun filtered -> 
                (filtered, sumOfAbsoluteValueOfFilteredScanline filtered))
        |> Seq.minBy (fun (_, sum) -> sum)

    filtered


/// <summary>
/// Filters the provided sequence of scanlines according to the PNG filtering 
/// mechanism.
/// </summary>
/// <param name="scanlines">A sequence of scanlines.</param>
/// <returns>
/// A sequence of filtered scanlines. Each filtered scanline corresponds to an
/// original scanline.
/// </returns>
let filterScanlines 
    (filter: ScanlineFilter)
    (bpp: int)
    (scanlines: Scanline[])
    : FilteredScanline[]=

    [| 
        for scanlineIndex in 0 .. scanlines.Length - 1 do
            let prevScanline =
                match scanlineIndex with
                | 0 -> None
                | _ -> Some scanlines.[scanlineIndex - 1]

            let scanline = scanlines.[scanlineIndex]
            yield filter bpp prevScanline scanline
    |]


let unfilterScanlines bpp (filteredScanlines: FilteredScanline[]): Scanline[] =
    [| 
        let mutable prevScanline = None
        for scanlineIndex in 0 .. filteredScanlines.Length - 1 do
            let filteredScanline = filteredScanlines.[scanlineIndex]

            let filterType = (int)filteredScanline.[0]
            match filterType with
            | f when f >= 0 && f < allPngUnfilters.Length -> 
                let unfilteredScanline = 
                    allPngUnfilters.[filterType] bpp prevScanline filteredScanline
                prevScanline <- Some unfilteredScanline
                yield unfilteredScanline
            | _ -> 
                invalidOp (sprintf "Unsupported PNG filter type %d" filterType)
    |]
