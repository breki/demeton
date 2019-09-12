module Demeton.Tests.``PNG filtering``

open Demeton.PngTypes
open Demeton.PngFilters

open FsUnit
open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote


[<Fact>]
let ``Can filter scanlines``() =
    let scanlines = [|
        [| 0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy |];
        [| 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy; 10uy |]
    |]
    let bpp = 8

    let filteredScanlines 
        = filterScanlines minSumOfAbsoluteDifferencesSelector bpp scanlines
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

type ScanlinesPairPropertyAttribute() = 
    inherit PropertyAttribute
        (Arbitrary = [| typeof<ScanlinesGenerator> |],
        QuietOnSuccess = true)

[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using None filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) = 

    let (bpp, scanline, prevScanline) = scanlines

    let (filtered, _) = 
        filterScanline 
            filterTypeNone ((byte)FilterType.FilterNone) bpp prevScanline scanline
    
    unfilterScanlineNone bpp prevScanline filtered = scanline 


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Sub filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) = 
    let (bpp, scanline, prevScanline) = scanlines
    let (filtered, _) = 
        filterScanline 
            filterTypeSub ((byte)FilterType.FilterSub) bpp prevScanline scanline
    let unfilteredScanline = unfilterScanlineSub bpp prevScanline filtered
    unfilteredScanline = scanline 


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Byte array returned by Up filter always contains filter type as first byte`` 
        (scanlines: ScanlinesPair) =

    let (bpp, scanline, prevScanline) = scanlines

    let (filtered, _) = 
        filterScanline 
            filterTypeUp ((byte)FilterType.FilterUp) bpp prevScanline scanline

    filtered.Length >= 1 && filtered.[0] = (byte)FilterType.FilterUp


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Up filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) =
    let (bpp, scanline, prevScanline) = scanlines
    let (filtered, _) = 
        filterScanline 
            filterTypeUp ((byte)FilterType.FilterUp) bpp prevScanline scanline
    unfilterScanlineUp bpp prevScanline filtered = scanline 


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Average filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) =

    let (bpp, scanline, prevScanline) = scanlines

    let (filtered, _) = 
        filterScanline 
            filterTypeAverage 
            ((byte)FilterType.FilterAverage) bpp prevScanline scanline

    let unfilteredScanline = unfilterScanlineAverage bpp prevScanline filtered
    printf "unfilteredScanline: %A\n" unfilteredScanline
    unfilteredScanline = scanline 


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Paeth filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) =

    let (bpp, scanline, prevScanline) = scanlines

    let (filtered, _) = 
        filterScanline 
            filterTypePaeth 
            ((byte)FilterType.FilterPaeth) bpp prevScanline scanline

    let unfilteredScanline = unfilterScanlinePaeth bpp prevScanline filtered 
    unfilteredScanline = scanline 


[<Property>]
[<Trait("Category", "integration")>]
let ``Unfiltering scanlines returns the original scanlines``
    (scanlines2d: byte[,]) =

    let bpp = 8

    let toJaggedArray (arr2d: 'T[,]): 'T[][] =
        [|
            for row in 0 .. (Array2D.length2 arr2d - 1) ->
                arr2d.[0..(Array2D.length1 arr2d - 1), row]
        |]

    let scanlines = toJaggedArray scanlines2d
    let filteredScanlines = 
        filterScanlines minSumOfAbsoluteDifferencesSelector bpp scanlines
    let unfilteredScanlines = unfilterScanlines bpp filteredScanlines
    scanlines = unfilteredScanlines
