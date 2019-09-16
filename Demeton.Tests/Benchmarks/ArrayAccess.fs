module Demeton.Benchmarks.ArrayAccess

open BenchmarkDotNet.Attributes
open System

type ArrayAccessComparison() = 
    [<Params (100, 1000, 10000, 100000)>] 
    member val public ArraySize = 0 with get, set

    member self.PrepareArray() =
        let rnd = new System.Random(234)

        Array.init self.ArraySize (fun i -> byte (rnd.Next(255)))

    [<Benchmark>]
    member self.AccessArrayViaIndexer() =
        let bytes = self.PrepareArray()

        let mutable sum = 0
        for i in 0 .. (bytes.Length-1) do
            let value = bytes.[i]
            sum <- sum + int value

    [<Benchmark>]
    member self.AccessArrayThroughSpan() =
        let bytes = self.PrepareArray()

        let bytesSpan = new Span<byte>(bytes)

        let mutable sum = 0
        for i in 0 .. (bytes.Length-1) do
            let value = bytesSpan.[i]
            sum <- sum + int value
