module Demeton.Tests.``Reading binary streams``

open FsCheck.Xunit

open System.IO


[<Property>]
let ``Reading of big endian int is inverse of writing it`` (value: int) = 
    use stream = new MemoryStream()
    stream |> Bnry.writeBigEndianInt32 value |> ignore
    stream.Seek (0L, SeekOrigin.Begin) |> ignore
    Bnry.readBigEndianInt32 stream = value 

[<Property>]
let ``Reading of big endian uint is inverse of writing it`` (value: uint32) = 
    use stream = new MemoryStream()
    stream |> Bnry.writeBigEndianUInt32 value |> ignore
    stream.Seek (0L, SeekOrigin.Begin) |> ignore
    Bnry.readBigEndianUInt32 stream = value 
