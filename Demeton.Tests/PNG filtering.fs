module Demeton.Tests.``PNG filtering``

open FsUnit
open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

type ScanlineFilter = byte[] option -> byte[] -> byte[]

type FilterType = 
    FilterNone = 0uy
    | FilterSub = 1uy
    | FilterUp = 2uy
    | FilterAverage = 3uy
    | FilterPaeth = 4uy


let filterScanlineNone _ (scanline: byte[]): byte[] =    
        [| 
            for i in 0 .. scanline.Length -> 
                match i with
                | 0 -> (byte)FilterType.FilterNone
                | x -> scanline.[x - 1]
        |]


let unfilterScanlineNone _ (filtered: byte[]) =
    printf "%A\n" filtered
    [| for i in 0 .. filtered.Length - 2 -> filtered.[i + 1] |]


let filterScanlineSub _ (scanline: byte[]): byte[] = 
        [| 
            for i in 0 .. scanline.Length -> 
                match i with
                | 0 -> (byte)FilterType.FilterSub
                | 1 -> scanline.[0]
                | x -> scanline.[x-1] - scanline.[x-2]
        |]


let unfilterScanlineSub _ (filtered: byte[]): byte[] =
    let scanlineLength = filtered.Length - 1
    let scanline: byte[] = Array.zeroCreate scanlineLength

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


let filterScanlineUp (prevScanline: byte[] option) (scanline: byte[]): byte[] = 
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


let unfilterScanlineUp (prevScanline: byte[] option) (filtered: byte[]): byte[] =
    let scanlineLength = filtered.Length - 1

    match scanlineLength with
    | l when (l < 0) -> 
        invalidArg "filtered byte array cannot be empty" "filtered"
    | 0 -> [||]
    | _ -> 
        match prevScanline with
        | None -> filtered |> Array.skip 1
        | Some prev -> 
            let scanline: byte[] = Array.zeroCreate scanlineLength

            for i in 1 .. scanlineLength do
                scanline.[i-1] <- filtered.[i] + prev.[i-1]

            scanline


/// <summary>
/// Filters the provided sequence of scanlines according to the PNG filtering 
/// mechanism.
/// </summary>
/// <param name="scanlines">A sequence of scanlines.</param>
/// <returns>
/// A sequence of filtered scanlines. Each filtered scanline corresponds to an
/// original scanline.
/// </returns>
let filterScanlines (scanlines: byte[] seq): byte[] seq =
    // https://www.w3.org/TR/PNG/#9Filters
    Seq.empty


[<Fact(Skip="todo: we need to implement filter types first")>]
let ``Can filter scanlines``() =
    let scanlines = [|
        [| 0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy |];
        [| 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy; 10uy |]
    |]

    let filteredScanlines = filterScanlines scanlines
    test <@ filteredScanlines |> Seq.length = 2 @>
    test <@ filteredScanlines |> Seq.exists (fun sc -> sc.Length <> 11) |> not @>


type ScanlinesPair = (byte[] * byte[] option)
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

        printf "%A" scanline
        printf "%A" prevScanline

        let filtered = filterScanlineUp prevScanline scanline

        unfilterScanlineUp prevScanline filtered = scanline 
