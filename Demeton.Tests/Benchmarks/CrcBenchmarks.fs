module Demeton.Benchmarks.CrcBenchmarks

open Demeton.CRC

open BenchmarkDotNet.Attributes

type Crc32Comparison() = 
    let mutable array: byte[] = [||]

    [<Params (25920000)>] 
    member val public ArraySize = 0 with get, set

    [<IterationSetup>]
    member self.CreateArray(): unit = 
        let rnd = new System.Random(34545)
        array <- Array.init self.ArraySize (fun _ -> byte (rnd.Next(256)))

    [<Benchmark>]
    member self.Crc1() = crc32 array

    [<Benchmark>]
    member self.Crc2() = crc32_2 array

    [<Benchmark>]
    member self.Crc3() = crc32_3 array

    [<Benchmark>]
    member self.Crc4() = crc32_4 array

    [<Benchmark>]
    member self.Crc5() = crc32_5 array

    [<Benchmark>]
    member self.Crc6() = crc32_6 array
