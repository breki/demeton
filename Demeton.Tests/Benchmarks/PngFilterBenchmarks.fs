﻿module Demeton.Benchmarks.PngFilterBenchmarks

open Demeton.PngTypes
open Demeton.PngFiltering

open BenchmarkDotNet.Attributes

type PngFilterComparison() = 
    let mutable imageData: ImageData = [||]

    [<Params (2000)>] 
    member val public ImageSize = 0 with get, set

    [<IterationSetup>]
    member self.CreateImageData(): unit = 
        let rnd = new System.Random(34545)
        imageData <- 
            Array.init 
                (self.ImageSize * self.ImageSize * 2) 
                (fun _ -> byte (rnd.Next(256)))

    [<Benchmark>]
    member self.FilterScanlines() = 
        filterScanlines self.ImageSize self.ImageSize 16 imageData