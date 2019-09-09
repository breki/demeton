module Demeton.Tests.``PNG filtering``

open FsUnit
open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

type Scanline = byte[]
type FilteredScanline = byte[]

type ScanlineFilter = Scanline option -> Scanline -> FilteredScanline
type ScanlineUnfilter = Scanline option -> FilteredScanline -> Scanline

type FilterType = 
    FilterNone = 0uy
    | FilterSub = 1uy
    | FilterUp = 2uy
    | FilterAverage = 3uy
    | FilterPaeth = 4uy

let filterScanlineNone _ (scanline: Scanline): FilteredScanline =    
        [| 
            for i in 0 .. scanline.Length -> 
                match i with
                | 0 -> (byte)FilterType.FilterNone
                | x -> scanline.[x - 1]
        |]


let unfilterScanlineNone _ (filtered: FilteredScanline) =
    printf "%A\n" filtered
    [| for i in 0 .. filtered.Length - 2 -> filtered.[i + 1] |]


let filterScanlineSub _ (scanline: Scanline): FilteredScanline = 
    [| 
        for i in 0 .. scanline.Length -> 
            match i with
            | 0 -> (byte)FilterType.FilterSub
            | 1 -> scanline.[0]
            | x -> scanline.[x-1] - scanline.[x-2]
    |]


let unfilterScanlineSub _ (filtered: FilteredScanline): Scanline =
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


let allPngFilters = [|
    filterScanlineNone; filterScanlineSub; filterScanlineUp; 
    filterScanlineAverage; filterScanlinePaeth
|]


let allPngUnfilters = [|
    unfilterScanlineNone; unfilterScanlineSub; unfilterScanlineUp; 
    unfilterScanlineAverage; unfilterScanlinePaeth
|]


let sumOfAbsoluteValueOfFilteredScanline (filtered: FilteredScanline): int =
    filtered |> Array.map int |> Array.sum


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
    (filters: ScanlineFilter seq) (scanlines: Scanline[]): FilteredScanline[]=

    [| 
        for scanlineIndex in 0 .. scanlines.Length - 1 do
            let prevScanline =
                match scanlineIndex with
                | 0 -> None
                | _ -> Some scanlines.[scanlineIndex - 1]

            let scanline = scanlines.[scanlineIndex]

            let (filtered, _) = 
                filters 
                |> Seq.map (fun filter -> filter prevScanline scanline)
                |> Seq.map (fun filtered -> (filtered, sumOfAbsoluteValueOfFilteredScanline filtered))
                |> Seq.minBy (fun (_, sum) -> sum)

            yield filtered
    |]


let unfilterScanlines (filteredScanlines: FilteredScanline[]): Scanline[] =
    [| 
        let mutable prevScanline = None
        for scanlineIndex in 0 .. filteredScanlines.Length - 1 do
            let filteredScanline = filteredScanlines.[scanlineIndex]

            let filterType = (int)filteredScanline.[0]
            match filterType with
            | f when f >= 0 && f < allPngUnfilters.Length -> 
                let unfilteredScanline = 
                    allPngUnfilters.[filterType] prevScanline filteredScanline
                prevScanline <- Some unfilteredScanline
                yield unfilteredScanline
            | _ -> 
                invalidOp (sprintf "Unsupported PNG filter type %d" filterType)
    |]


[<Fact>]
let ``Can filter scanlines``() =
    let scanlines = [|
        [| 0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy |];
        [| 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy; 10uy |]
    |]

    let filteredScanlines = filterScanlines allPngFilters scanlines
    test <@ filteredScanlines |> Seq.length = 2 @>
    test <@ filteredScanlines |> Seq.exists (fun sc -> sc.Length <> 11) |> not @>


type ScanlinesPair = (Scanline * Scanline option)
type ScanlinesGenerator =

    static member ScanlinePair() =
        let scanlineLength = 20
        let randomGrayscaleValue = Arb.generate<byte>

        // first build a generator for the main scanline
        let scanline = randomGrayscaleValue 
                        |> Gen.arrayOfLength scanlineLength

        // now we build the generator for the previous scanline... 
        let prevScanline = 
            randomGrayscaleValue 
            |> Gen.arrayOfLength scanlineLength 
            // ... (with 20% probability that it will be None)
            |> Gen.optionOf

        // now we combine the two generators into a pair...
        Gen.zip scanline prevScanline
        // ... and then map it into ScanlinesPair
        |> Gen.map (fun (s, p) -> ScanlinesPair(s, p))
        |> Arb.fromGen


type ``PNG filtering property tests``() = 
    do 
        Arb.register<ScanlinesGenerator>() |> ignore

    [<Property>]
    [<Trait("Category", "properties")>]
    member __.``Filtering and unfiltering using None filter type returns the same scanline`` 
            (scanlines: ScanlinesPair) = 

        let (scanline, prevScanline) = scanlines

        let filtered = filterScanlineNone prevScanline scanline
    
        unfilterScanlineNone prevScanline filtered = scanline 


    [<Property>]
    [<Trait("Category", "properties")>]
    member __.``Filtering and unfiltering using Sub filter type returns the same scanline`` 
            (scanlines: ScanlinesPair) = 

        let (scanline, prevScanline) = scanlines

        let filtered = filterScanlineSub prevScanline scanline
    
        unfilterScanlineSub prevScanline filtered = scanline 


    [<Property>]
    [<Trait("Category", "properties")>]
    member __.``Byte array returned by Up filter always contains filter type as first byte`` 
            (scanlines: ScanlinesPair) =

        let (scanline, prevScanline) = scanlines

        let filtered = filterScanlineUp prevScanline scanline

        printf "filtered: %A\n" filtered

        filtered.Length >= 1 && filtered.[0] = (byte)FilterType.FilterUp


    [<Property>]
    [<Trait("Category", "properties")>]
    member __.``Filtering and unfiltering using Up filter type returns the same scanline`` 
            (scanlines: ScanlinesPair) =

        let (scanline, prevScanline) = scanlines

        let filtered = filterScanlineUp prevScanline scanline

        unfilterScanlineUp prevScanline filtered = scanline 


    [<Property>]
    [<Trait("Category", "properties")>]
    member __.``Filtering and unfiltering using Average filter type returns the same scanline`` 
            (scanlines: ScanlinesPair) =

        let (scanline, prevScanline) = scanlines

        let filtered = filterScanlineAverage prevScanline scanline

        unfilterScanlineAverage prevScanline filtered = scanline 


    [<Property>]
    [<Trait("Category", "properties")>]
    member __.``Filtering and unfiltering using Paeth filter type returns the same scanline`` 
            (scanlines: ScanlinesPair) =

        let (scanline, prevScanline) = scanlines

        let filtered = filterScanlinePaeth prevScanline scanline

        unfilterScanlinePaeth prevScanline filtered = scanline 

    [<Property>]
    [<Trait("Category", "properties")>]
    member __.``Calculates sumOfAbsoluteValueOfFilteredScanline correctly``
        (b1: byte) (b2: byte) (b3: byte) =
        [| b1; b2; b3 |] |> sumOfAbsoluteValueOfFilteredScanline
            = ((int)b1) + ((int)b2) + ((int)b3)

    [<Property>]
    [<Trait("Category", "integration")>]
    member __.``Unfiltering scanlines returns the original scanlines``
        (scanlines2d: byte[,]) =
        printf "Try: %A\n" scanlines2d

        let toJaggedArray (arr2d: 'T[,]): 'T[][] =
            [|
                for row in 0 .. (Array2D.length2 arr2d - 1) ->
                    arr2d.[0..(Array2D.length1 arr2d - 1), row]
            |]

        let scanlines = toJaggedArray scanlines2d
        let filteredScanlines = 
            filterScanlines allPngFilters scanlines
        let unfilteredScanlines = unfilterScanlines filteredScanlines
        scanlines = unfilteredScanlines
