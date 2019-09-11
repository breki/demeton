module Demeton.PngFilters

open Demeton.PngTypes


let filterScanlineNone bpp _ (scanline: Scanline): FilteredScanline =    
        [| 
            for i in 0 .. scanline.Length -> 
                match i with
                | 0 -> (byte)FilterType.FilterNone
                | x -> scanline.[x - 1]
        |]


let unfilterScanlineNone bpp _ (filtered: FilteredScanline) =
    [| for i in 0 .. filtered.Length - 2 -> filtered.[i + 1] |]


let filterScanlineSub bpp _ (scanline: Scanline): FilteredScanline = 
    [| 
        for i in 0 .. scanline.Length -> 
            match i with
            | 0 -> (byte)FilterType.FilterSub
            | 1 -> scanline.[0]
            | x -> scanline.[x-1] - scanline.[x-2]
    |]


let unfilterScanlineSub bpp _ (filtered: FilteredScanline): Scanline =
    let scanlineLength = filtered.Length - 1
    let scanline = Array.zeroCreate scanlineLength

    match scanlineLength with
    | 0 -> scanline
    | _ -> 
        let mutable lastValue = filtered.[1]
        scanline.[0] <- lastValue

        for i in 1 .. scanlineLength-1 do
            let value = lastValue + filtered.[i + 1] 
            scanline.[i] <- value
            lastValue <- value

        scanline


let filterScanlineUp 
    bpp
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


let unfilterScanlineUp 
    bpp
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


let filterScanlineAverage 
    bpp
    (prevScanline: Scanline option) (scanline: Scanline): FilteredScanline = 
    [| 
        for i in 0 .. scanline.Length -> 
            match i with
            | 0 -> (byte)FilterType.FilterAverage
            | x -> 
                let currentValue = scanline.[x-1]
                let prevValue = 
                    match x with
                    | 1 -> 0uy
                    | _ -> scanline.[x-2]
                let prevScanlineValue = 
                    match prevScanline with
                    | None -> 0uy
                    | Some prev -> prev.[x-1]
                currentValue 
                    - (byte)(((int)prevValue + (int)prevScanlineValue) / 2)
    |]


let unfilterScanlineAverage 
    bpp
    (prevScanline: Scanline option) (filtered: FilteredScanline): Scanline =
    [|
        let mutable lastValue = filtered.[1]
        
        for i in 0 .. filtered.Length - 2 ->
            let currentValue = filtered.[i + 1]
            let prevValue = 
                match i with
                | 0 -> 0uy
                | _ -> lastValue
            let prevScanlineValue =
                match prevScanline with
                | None -> 0uy
                | Some prev -> prev.[i]

            let unfilteredValue = 
                currentValue 
                    + (byte)((((int)prevValue + (int)prevScanlineValue)) / 2)
            lastValue <- unfilteredValue
            unfilteredValue
    |]


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
