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

    let filteredScanlines 
        = filterScanlines minSumOfAbsoluteValueSelector scanlines
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

type ScanlinesPairPropertyAttribute() = 
    inherit PropertyAttribute
        (Arbitrary = [| typeof<ScanlinesGenerator> |],
        QuietOnSuccess = true)

[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using None filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) = 

    let (scanline, prevScanline) = scanlines

    let filtered = filterScanlineNone prevScanline scanline
    
    unfilterScanlineNone prevScanline filtered = scanline 


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Sub filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) = 

    let (scanline, prevScanline) = scanlines

    let filtered = filterScanlineSub prevScanline scanline
    
    unfilterScanlineSub prevScanline filtered = scanline 


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Byte array returned by Up filter always contains filter type as first byte`` 
        (scanlines: ScanlinesPair) =

    let (scanline, prevScanline) = scanlines

    let filtered = filterScanlineUp prevScanline scanline

    printf "filtered: %A\n" filtered

    filtered.Length >= 1 && filtered.[0] = (byte)FilterType.FilterUp


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Up filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) =

    let (scanline, prevScanline) = scanlines

    let filtered = filterScanlineUp prevScanline scanline

    unfilterScanlineUp prevScanline filtered = scanline 


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Average filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) =

    let (scanline, prevScanline) = scanlines

    let filtered = filterScanlineAverage prevScanline scanline

    unfilterScanlineAverage prevScanline filtered = scanline 


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Paeth filter type returns the same scanline`` 
        (scanlines: ScanlinesPair) =

    let (scanline, prevScanline) = scanlines

    let filtered = filterScanlinePaeth prevScanline scanline

    unfilterScanlinePaeth prevScanline filtered = scanline 

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
    printf "Try: %A\n" scanlines2d

    let toJaggedArray (arr2d: 'T[,]): 'T[][] =
        [|
            for row in 0 .. (Array2D.length2 arr2d - 1) ->
                arr2d.[0..(Array2D.length1 arr2d - 1), row]
        |]

    let scanlines = toJaggedArray scanlines2d
    let filteredScanlines = 
        filterScanlines minSumOfAbsoluteValueSelector scanlines
    let unfilteredScanlines = unfilterScanlines filteredScanlines
    scanlines = unfilteredScanlines
