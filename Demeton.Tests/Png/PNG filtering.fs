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
        = filterScanlines minSumOfAbsoluteValueSelector bpp scanlines
    test <@ filteredScanlines |> Seq.length = 2 @>
    test <@ filteredScanlines |> Seq.exists (fun sc -> sc.Length <> 11) |> not @>


type ScanlinesPair = (int * Scanline * Scanline option)
type ScanlinesGenerator =
    static member ScanlinesPair() =
        // a generator for bits-per-pixel value
        //let bppValue = Gen.elements [| 8; 16; 24; 32 |]
        // todo return back all the elements
        let bppValue = Gen.elements [| 16 |]

        // note that the scanline length must be divisible with 3 and 4 
        // (as we use 3 and 4 as bytes-per-pixel values in this generator)
        // todo return back all the elements
        //let scanlineLength = 12
        let scanlineLength = 4
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
        Gen.zip3 bppValue scanline prevScanline
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

    let filtered = filterScanlineNone 1 prevScanline scanline
    
    unfilterScanlineNone 1 prevScanline filtered = scanline 


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Sub filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) = 
    let (bpp, scanline, prevScanline) = scanlines
    let filtered = filterScanlineSub bpp prevScanline scanline
    let unfilteredScanline = unfilterScanlineSub bpp prevScanline filtered
    unfilteredScanline = scanline 


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Byte array returned by Up filter always contains filter type as first byte`` 
        (scanlines: ScanlinesPair) =

    let (bpp, scanline, prevScanline) = scanlines

    let filtered = filterScanlineUp bpp prevScanline scanline

    filtered.Length >= 1 && filtered.[0] = (byte)FilterType.FilterUp


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Up filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) =
    let (bpp, scanline, prevScanline) = scanlines
    let filtered = filterScanlineUp bpp prevScanline scanline
    unfilterScanlineUp bpp prevScanline filtered = scanline 


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Average filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) =

    let (bpp, scanline, prevScanline) = scanlines
    printf "bpp: %d\n" bpp
    printf "prev scanline: %A\n" prevScanline 
    printf "scanline: %A\n" scanline 

    let filtered = filterScanlineAverage bpp prevScanline scanline
    printf "filtered: %A\n" filtered

    let unfilteredScanline = unfilterScanlineAverage bpp prevScanline filtered
    printf "unfilteredScanline: %A\n" unfilteredScanline
    unfilteredScanline = scanline 


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Paeth filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) =

    let (bpp, scanline, prevScanline) = scanlines

    let filtered = filterScanlinePaeth bpp prevScanline scanline

    unfilterScanlinePaeth bpp prevScanline filtered = scanline 

[<Property>]
[<Trait("Category", "properties")>]
let ``Calculates sumOfAbsoluteValueOfFilteredScanline correctly``
    (b1: byte) (b2: byte) (b3: byte) =
    [| b1; b2; b3 |] |> sumOfAbsoluteValueOfFilteredScanline
        = ((int)b1) + ((int)b2) + ((int)b3)

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
        filterScanlines minSumOfAbsoluteValueSelector bpp scanlines
    let unfilteredScanlines = unfilterScanlines bpp filteredScanlines
    scanlines = unfilteredScanlines
