module Demeton.Benchmarks.CrcBenchmarks

open BenchmarkDotNet.Attributes

type Crc32Comparison() =
    let mutable array: byte[] = [||]

    [<Params 20000000>]
    member val public ArraySize = 0 with get, set

    [<IterationSetup>]
    member self.CreateArray() : unit =
        let rnd = System.Random(34545)
        array <- Array.init self.ArraySize (fun _ -> byte (rnd.Next(256)))

    [<Benchmark>]
    member self.Crc() = Crc.crc32 array
