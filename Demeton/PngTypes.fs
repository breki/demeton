module Demeton.PngTypes

type PngBitDepth = 
    BitDepth1 = 1uy 
    | BitDepth2 = 2uy
    | BitDepth4 = 4uy
    | BitDepth8 = 8uy
    | BitDepth16 = 16uy

type PngColorType =
    Grayscale = 0uy
    | Rgb = 2uy
    | Indexed = 3uy
    | GrayscaleAlpha = 4uy
    | RgbAlpha = 6uy

type PngCompressionMethod = 
    DeflateInflate = 0uy

type PngFilterMethod = 
    AdaptiveFiltering = 0uy

type PngInterlaceMethod =
    NoInterlace = 0uy
    | Adam7Interlace = 1uy

[<Struct>]
type ChunkType = 
    val TypeName: string
    new (typeName: string) = 
        { 
            TypeName = 
                if typeName.Length <> 4 
                    then invalidArg "typeName" "PNG chunk type must be 4 characters long."
                else typeName
        }

type ChunkDataWriter = unit -> byte[]

type IhdrData = {
        Width: int
        Height: int
        BitDepth: PngBitDepth
        ColorType: PngColorType
        InterlaceMethod: PngInterlaceMethod
    }

type Grayscale8BitImageData = byte[,]

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
