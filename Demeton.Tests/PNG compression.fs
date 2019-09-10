module Demeton.Tests.``PNG compression``

open System.IO
open ICSharpCode.SharpZipLib.Zip.Compression

open FsUnit
open FsCheck
open FsCheck.Xunit
open Xunit

// https://www.nuget.org/packages/Iconic.Zlib.Netstandard/
// https://stackoverflow.com/questions/741591/how-to-use-icsharpcode-ziplib-with-stream
// todo: remove this when we get the deflating/inflating thing running
//[<Fact>]
//[<Trait("Category", "integration")>]
//let ``Inflating a deflated data returns the original data``() =
//    let originalData = [| 10uy; 20uy; 30uy; 10uy; 20uy; 30uy; 10uy; 20uy; 30uy; 10uy; 20uy; 30uy; |]
//    use originalDataStream = new MemoryStream(originalData)

//    use compressedDataStream = new MemoryStream()
//    use deflaterStream: DeflaterOutputStream =
//        new DeflaterOutputStream(compressedDataStream)
//    originalDataStream.CopyTo(deflaterStream)
//    deflaterStream.Flush()
//    compressedDataStream.Flush()

//    let compressedData = compressedDataStream.ToArray()
//    printf "compressed data: %A\n" compressedData

//    use compressedDataInputStream = new MemoryStream(compressedData)
//    use inflaterStream = new InflaterInputStream(compressedDataInputStream)
    
//    use decompressedStream = new MemoryStream()
//    inflaterStream.CopyTo(decompressedStream)

//    let decompressedData = decompressedStream.ToArray()

//    test <@ decompressedData = originalData @>
    
// https://csharp.hotexamples.com/examples/ICSharpCode.SharpZipLib.Zip.Compression/Deflater/-/php-deflater-class-examples.html
[<Property>]
[<Trait("Category", "integration")>]
let ``Inflating a deflated data returns the original data``
    (originalData: byte[]) =
    printf "original data: %A\n" originalData
   
    let deflater = new Deflater()
    deflater.SetInput(originalData)
    deflater.Finish()

    use compressedOutputStream = new MemoryStream()
    while not deflater.IsFinished do
        let compressionBuffer: byte[] = Array.zeroCreate (100 * 1024 * 4)

        let producedBytesCount = deflater.Deflate(compressionBuffer)
        compressedOutputStream.Write(compressionBuffer, 0, producedBytesCount)

    let compressedData = compressedOutputStream.ToArray()
    printf "compressed data: %A\n" compressedData

    let inflater = new Inflater()
    inflater.SetInput(compressedData)
    
    use decompressedOutputStream = new MemoryStream()
    while not inflater.IsFinished do
        let inflaterBuffer: byte[] = Array.zeroCreate (100 * 1024 * 4)
        let producedBytesCount = inflater.Inflate(inflaterBuffer)
        decompressedOutputStream.Write(inflaterBuffer, 0, producedBytesCount)

    let decompressedData = decompressedOutputStream.ToArray()
    printf "decompressed data: %A\n" decompressedData
    

