module Demeton.Tests.``PNG compression``

open Demeton.PngChunks

open System.IO

open FsUnit
open FsCheck
open FsCheck.Xunit
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
    

