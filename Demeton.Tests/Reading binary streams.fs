module Demeton.Tests.``Reading binary streams``

open Demeton.Binary

open FsCheck.Xunit

open System.IO


[<Property>]
let ``Reading of big endian int is inverse of writing it`` (value: int) = 
    use stream = new MemoryStream()
    stream |> writeBigEndianInt32 value |> ignore
    stream.Seek (0L, SeekOrigin.Begin) |> ignore
    readBigEndianInt32 stream = value 

[<Property>]
let ``Reading of big endian uint is inverse of writing it`` (value: uint32) = 
    use stream = new MemoryStream()
    stream |> writeBigEndianUInt32 value |> ignore
    stream.Seek (0L, SeekOrigin.Begin) |> ignore
    readBigEndianUInt32 stream = value 
