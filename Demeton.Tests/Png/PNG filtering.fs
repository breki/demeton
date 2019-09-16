module Demeton.Tests.``PNG filtering``

open Demeton.PngTypes
open Demeton.PngFilters

open FsUnit
open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

open System

[<Fact>]
let ``Can filter scanlines``() =
    let scanlines = [|
        [| 0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy |];
        [| 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy; 10uy |]
    |]
    let bpp = 8

    let filteredScanlines 
        //= filterScanlines minSumOfAbsoluteDifferencesSelector bpp scanlines
        = filterScanlines filterScanline bpp scanlines
    test <@ filteredScanlines |> Seq.length = 2 @>
    test <@ filteredScanlines |> Seq.exists (fun sc -> sc.Length <> 11) |> not @>


type ScanlinesPair = (int * Scanline * Scanline option)
type ScanlinesGenerator =
    static member ScanlinesPair() =
        // a generator for bits-per-pixel value
        let bytesPerPixel = Gen.elements [| 1; 2; 3; 4 |]

        // note that the scanline length must be divisible with 3 and 4 
        // (as we use 3 and 4 as bytes-per-pixel values in this generator)
        let scanlineLength = 12
        let randomByteValue = Arb.generate<byte>

        // a generator for the main scanline
        let scanline = randomByteValue 
                        |> Gen.arrayOfLength scanlineLength

        // now we build the generator for the previous scanline... 
        let prevScanline = 
            randomByteValue 
            |> Gen.arrayOfLength scanlineLength 
            // ... (with 20% probability that it will be None)
            |> Gen.optionOf

        // we combine the three generators into a tuple...
        Gen.zip3 bytesPerPixel scanline prevScanline
        // ... and then map it into ScanlinesPair
        |> Gen.map (fun (bpp, s, p) -> ScanlinesPair(bpp, s, p))
        |> Arb.fromGen


let filterScanlineUsingFilterType
    filterTypeFunc
    filterTypeByte
    bytesPP 
    (prevScanline: Scanline option) 
    (scanline: Scanline) 
    : FilteredScanline = 
    let filteredScanline = Array.zeroCreate (scanline.Length+1)
    filteredScanline.[0] <- filterTypeByte

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
        filteredScanline.[filteredScanlineIndex] <- 
            filterTypeFunc raw left up upLeft

    filteredScanline


type ScanlinesPairPropertyAttribute() = 
    inherit PropertyAttribute
        (Arbitrary = [| typeof<ScanlinesGenerator> |],
        QuietOnSuccess = true)

[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using None filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) = 

    let (bpp, scanline, prevScanline) = scanlines

    let filtered = 
        filterScanlineUsingFilterType 
            (fun r l u ul -> r) 
            ((byte)FilterType.FilterNone) 
            bpp 
            prevScanline 
            scanline
    let filteredSpan = new Span<byte>(filtered, 1, filtered.Length - 1)

    let unfilteredSpan = 
        new Span<byte>(Array.zeroCreate scanline.Length)

    unfilterScanlineNone filteredSpan unfilteredSpan

    unfilteredSpan.ToArray() = scanline 


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Sub filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) = 
    let (bpp, scanline, prevScanline) = scanlines

    let filtered = 
        filterScanlineUsingFilterType 
            filterTypeSub 
            ((byte)FilterType.FilterSub) 
            bpp 
            prevScanline 
            scanline

    let filteredSpan = new Span<byte>(filtered, 1, filtered.Length - 1)

    let unfilteredSpan = 
        new Span<byte>(Array.zeroCreate scanline.Length)

    unfilterScanlineSub bpp filteredSpan unfilteredSpan
    
    unfilteredSpan.ToArray() = scanline 


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Up filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) =
    let (bpp, scanline, prevScanline) = scanlines
    let filtered = 
        filterScanlineUsingFilterType 
            filterTypeUp 
            ((byte)FilterType.FilterUp) 
            bpp 
            prevScanline 
            scanline

    let prevScanlineSpan =
        match prevScanline with
        | None -> new Span<byte>()
        | Some prev -> new Span<byte>(prev)

    let filteredSpan = new Span<byte>(filtered, 1, filtered.Length - 1)

    let unfilteredSpan = 
        new Span<byte>(Array.zeroCreate scanline.Length)

    unfilterScanlineUp bpp prevScanlineSpan filteredSpan unfilteredSpan

    unfilteredSpan.ToArray() = scanline 


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Average filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) =

    let (bpp, scanline, prevScanline) = scanlines

    let filtered = 
        filterScanlineUsingFilterType 
            filterTypeAverage 
            ((byte)FilterType.FilterAverage) 
            bpp 
            prevScanline 
            scanline

    let prevScanlineSpan =
        match prevScanline with
        | None -> new Span<byte>()
        | Some prev -> new Span<byte>(prev)

    let filteredSpan = new Span<byte>(filtered, 1, filtered.Length - 1)

    let unfilteredSpan = 
        new Span<byte>(Array.zeroCreate scanline.Length)

    unfilterScanlineAverage bpp prevScanlineSpan filteredSpan unfilteredSpan
    
    unfilteredSpan.ToArray() = scanline 


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Paeth filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) =

    let (bpp, scanline, prevScanline) = scanlines

    let filtered = 
        filterScanlineUsingFilterType 
            filterTypePaeth 
            ((byte)FilterType.FilterPaeth) 
            bpp 
            prevScanline 
            scanline
    
    let prevScanlineSpan =
        match prevScanline with
        | None -> new Span<byte>()
        | Some prev -> new Span<byte>(prev)

    let filteredSpan = new Span<byte>(filtered, 1, filtered.Length - 1)

    let unfilteredSpan = 
        new Span<byte>(Array.zeroCreate scanline.Length)

    unfilterScanlinePaeth bpp prevScanlineSpan filteredSpan unfilteredSpan
    
    unfilteredSpan.ToArray() = scanline

// todo fix this test once we move to spans for filtering, too
//[<Property>]
//[<Trait("Category", "integration")>]
//let ``Unfiltering scanlines returns the original scanlines``
//    (scanlines2d: byte[,]) =

//    let bpp = 8

//    let toJaggedArray (arr2d: 'T[,]): 'T[][] =
//        [|
//            for row in 0 .. (Array2D.length2 arr2d - 1) ->
//                arr2d.[0..(Array2D.length1 arr2d - 1), row]
//        |]

//    let scanlines = toJaggedArray scanlines2d
//    let filteredScanlines = 
//        filterScanlines filterScanline bpp scanlines
//    let unfilteredScanlines = unfilterScanlines bpp filteredScanlines
//    scanlines = unfilteredScanlines
