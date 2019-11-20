module Demeton.Tests.``IDAT compression``

open Png.Chunks
open Demeton.Srtm
open Demeton.Srtm.Funcs
open Demeton.Srtm.Png

open System.IO
open System.Reflection

open FsUnit
open FsCheck
open FsCheck.Xunit
open TestHelp
open Xunit


// https://csharp.hotexamples.com/examples/ICSharpCode.SharpZipLib.Zip.Compression/Deflater/-/php-deflater-class-examples.html
[<Property>]
[<Trait("Category", "integration")>]
let ``Inflating a deflated data returns the original data``
    (originalData: byte[]) =
   
    use compressedOutputStream = new MemoryStream()
    compress originalData compressedOutputStream
    let compressedData = compressedOutputStream.ToArray()

    use decompressedOutputStream = new MemoryStream()
    decompress compressedData decompressedOutputStream

    let decompressedData = decompressedOutputStream.ToArray()
    decompressedData = originalData
    

/// <summary>
/// Calculates the compression rate of the image data encoded from the sample
/// HGT tile. This test is used to check how the "height to raw 16 bit" mapping
/// affects the compression rate.
/// </summary>
[<Fact>]
[<Trait("Category", "slow")>]
let ``Determining the compression rate``() =
    let srtmTileId = "N46E015"
    let hgtFileNameOnly = srtmTileId + ".hgt"
    let tileId = parseTileName hgtFileNameOnly.[0..6]

    use hgtStream = sampleFileStream hgtFileNameOnly

    let clock = new System.Diagnostics.Stopwatch()
    clock.Start()

    printfn ("Reading the heights array...")
    
    let heightsArray = createSrtmTileFromStream 3600 tileId hgtStream

    printfn 
        "%d Encoding heights into a raw image data..." clock.ElapsedMilliseconds
    let imageData = 
        heightsArrayToImageData demHeightToUInt16Value heightsArray

    printfn 
        "%d Filtering and compressing the image data into PNG IDAT chunk..." 
        clock.ElapsedMilliseconds
    let compressed = 
        serializeIdatChunkData
            3600
            3600
            16
            imageData
    
    printfn 
        "%d Compression rate is %d%%..." 
        clock.ElapsedMilliseconds
        (compressed.Length * 100 / (3600 * 3600 * 2))

