module Demeton.Tests.``Reading HGT height values from a stream``

open Demeton.Srtm

open System.IO;

open FsUnit
open Xunit


[<Fact>]
let ``Can read SRTM heights``() =
    let stream = new MemoryStream([| 10uy; 0uy; 0uy; 0uy; 1uy; 1uy |])
    let heights = readSrtmHeightsFromStream stream |> Array.ofSeq

    heights |> should equal [| Some 2560s; Some 0s; Some 257s |]

[<Fact>]
let ``Can read null SRTM heights``() =
    let stream = new MemoryStream([| 0x80uy; 0uy; 10uy; 0uy |])
    let heights = readSrtmHeightsFromStream stream |> Array.ofSeq

    heights |> should equal [| None; Some 2560s |]

[<Fact>]
let ``Can handle negative SRTM heights``() =
    let stream = new MemoryStream([| 255uy; 0b10011100uy; 10uy; 0uy |])
    let heights = readSrtmHeightsFromStream stream |> Array.ofSeq

    heights |> should equal [| Some -100s; Some 2560s |]
