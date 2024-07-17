module Demeton.Tests.``PNG filtering``

open Png.Types
open Png.Filters
open Png.Unfilters

open FsUnit
open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

open System

[<Fact>]
let ``Can filter scanlines`` () =
    let rawImageData =
        [| 0uy
           1uy
           2uy
           3uy
           4uy
           5uy
           6uy
           7uy
           8uy
           9uy
           1uy
           2uy
           3uy
           4uy
           5uy
           6uy
           7uy
           8uy
           9uy
           10uy |]

    let bpp = 8

    let imageWidth = 10
    let imageHeight = 2

    let filteredImageData =
        filterScanlines imageWidth imageHeight bpp rawImageData

    test <@ filteredImageData.Length = (imageWidth + 1) * 2 @>


[<Fact>]
[<Trait("Category", "slow")>]
let ``Filtering large images`` () =
    let imageSize = 2000

    let rnd = Random(34545)

    let imageData =
        Array.init (imageSize * imageSize * 2) (fun _ -> byte (rnd.Next(256)))

    filterScanlines imageSize imageSize 16 imageData |> ignore


type ScanlinesPair = ScanlineBitDepthMode * byte[] * byte[] option

type ScanlinesGenerator =
    static member ScanlinesPair() =
        // a generator for bits-per-pixel value
        let bytesPerPixel =
            Gen.elements
                [| SubByteMode 1
                   SubByteMode 2
                   SubByteMode 4
                   ByteMode 1
                   ByteMode 2
                   ByteMode 3
                   ByteMode 4 |]

        // note that the scanline length must be divisible with 3 and 4
        // (as we use 3 and 4 as bytes-per-pixel values in this generator)
        let scanlineLength = 12
        let randomByteValue = Arb.generate<byte>

        // a generator for the main scanline
        let scanline = randomByteValue |> Gen.arrayOfLength scanlineLength

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
    inherit
        PropertyAttribute(
            Arbitrary = [| typeof<ScanlinesGenerator> |],
            QuietOnSuccess = true
        )


let filterScanlineUsingFilterType
    filterTypeFunc
    filterTypeByte
    scanlineBitDepthMode
    (prevScanline: Span<byte>)
    (scanline: Span<byte>)
    : FilteredScanline =
    let filteredScanline = Array.zeroCreate (scanline.Length + 1)
    filteredScanline.[0] <- filterTypeByte

    let leftOffset =
        match scanlineBitDepthMode with
        | ByteMode bytesPP -> bytesPP
        // for sub-byte mode, we always work on the whole (single) byte
        | SubByteMode _ -> 1

    for scanlineIndex in 0 .. (scanline.Length - 1) do
        let raw = scanline.[scanlineIndex]

        let mutable left = 0uy
        let mutable up = 0uy
        let mutable upLeft = 0uy

        if scanlineIndex >= leftOffset then
            left <- scanline.[scanlineIndex - leftOffset]

        if prevScanline.Length > 0 then
            up <- prevScanline.[scanlineIndex]

            if scanlineIndex >= leftOffset then
                upLeft <- prevScanline.[scanlineIndex - leftOffset]

        let filteredScanlineIndex = scanlineIndex + 1

        filteredScanline.[filteredScanlineIndex] <-
            filterTypeFunc raw left up upLeft

    filteredScanline


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using None filter type returns the same scanline``
    (scanlines: ScanlinesPair)
    =

    let scanlineBitDepthMode, scanline, prevScanline = scanlines

    let prevScanlineSpan =
        match prevScanline with
        | None -> Span<byte>()
        | Some prev -> Span<byte>(prev)

    let scanlineSpan = Span<byte>(scanline)

    let filtered =
        filterScanlineUsingFilterType
            (fun r _ _ _ -> r)
            (byte FilterType.FilterNone)
            scanlineBitDepthMode
            prevScanlineSpan
            scanlineSpan

    let filteredSpan = Span<byte>(filtered, 1, filtered.Length - 1)

    let unfilteredSpan = Span<byte>(Array.zeroCreate scanline.Length)

    unfilterScanlineNone filteredSpan unfilteredSpan

    unfilteredSpan.ToArray() = scanline


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Sub filter type returns the same scanline``
    (scanlines: ScanlinesPair)
    =
    let bpp, scanline, prevScanline = scanlines

    let prevScanlineSpan =
        match prevScanline with
        | None -> Span<byte>()
        | Some prev -> Span<byte>(prev)

    let scanlineSpan = Span<byte>(scanline)

    let filtered =
        filterScanlineUsingFilterType
            filterTypeSub
            (byte FilterType.FilterSub)
            bpp
            prevScanlineSpan
            scanlineSpan

    let filteredSpan = Span<byte>(filtered, 1, filtered.Length - 1)

    let unfilteredSpan = Span<byte>(Array.zeroCreate scanline.Length)

    unfilterScanlineSub bpp filteredSpan unfilteredSpan

    unfilteredSpan.ToArray() = scanline


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Up filter type returns the same scanline``
    (scanlines: ScanlinesPair)
    =
    let bpp, scanline, prevScanline = scanlines

    let prevScanlineSpan =
        match prevScanline with
        | None -> Span<byte>()
        | Some prev -> Span<byte>(prev)

    let scanlineSpan = Span<byte>(scanline)

    let filtered =
        filterScanlineUsingFilterType
            filterTypeUp
            (byte FilterType.FilterUp)
            bpp
            prevScanlineSpan
            scanlineSpan

    let prevScanlineSpan =
        match prevScanline with
        | None -> Span<byte>()
        | Some prev -> Span<byte>(prev)

    let filteredSpan = Span<byte>(filtered, 1, filtered.Length - 1)

    let unfilteredSpan = Span<byte>(Array.zeroCreate scanline.Length)

    unfilterScanlineUp bpp prevScanlineSpan filteredSpan unfilteredSpan

    unfilteredSpan.ToArray() = scanline


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Average filter type returns the same scanline``
    (scanlines: ScanlinesPair)
    =

    let bpp, scanline, prevScanline = scanlines

    let prevScanlineSpan =
        match prevScanline with
        | None -> Span<byte>()
        | Some prev -> Span<byte>(prev)

    let scanlineSpan = Span<byte>(scanline)

    let filtered =
        filterScanlineUsingFilterType
            filterTypeAverage
            (byte FilterType.FilterAverage)
            bpp
            prevScanlineSpan
            scanlineSpan

    let prevScanlineSpan =
        match prevScanline with
        | None -> Span<byte>()
        | Some prev -> Span<byte>(prev)

    let filteredSpan = Span<byte>(filtered, 1, filtered.Length - 1)

    let unfilteredSpan = Span<byte>(Array.zeroCreate scanline.Length)

    unfilterScanlineAverage bpp prevScanlineSpan filteredSpan unfilteredSpan

    unfilteredSpan.ToArray() = scanline


[<ScanlinesPairProperty>]
[<Trait("Category", "properties")>]
let ``Filtering and unfiltering using Paeth filter type returns the same scanline``
    (scanlines: ScanlinesPair)
    =

    let bpp, scanline, prevScanline = scanlines

    let prevScanlineSpan =
        match prevScanline with
        | None -> Span<byte>()
        | Some prev -> Span<byte>(prev)

    let scanlineSpan = Span<byte>(scanline)

    let filtered =
        filterScanlineUsingFilterType
            filterTypePaeth
            (byte FilterType.FilterPaeth)
            bpp
            prevScanlineSpan
            scanlineSpan

    let prevScanlineSpan =
        match prevScanline with
        | None -> Span<byte>()
        | Some prev -> Span<byte>(prev)

    let filteredSpan = Span<byte>(filtered, 1, filtered.Length - 1)

    let unfilteredSpan = Span<byte>(Array.zeroCreate scanline.Length)

    unfilterScanlinePaeth bpp prevScanlineSpan filteredSpan unfilteredSpan

    unfilteredSpan.ToArray() = scanline
