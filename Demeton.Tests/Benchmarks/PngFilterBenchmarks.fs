module Demeton.Benchmarks.PngFilterBenchmarks

open Demeton.PngTypes
open Demeton.PngFilters

open BenchmarkDotNet.Attributes

type PngFilterComparison() = 
    let mutable imageData: ImageData = [||]

    [<Params (5000)>] 
    member val public ImageSize = 0 with get, set

    [<IterationSetup>]
    member self.CreateImageData(): unit = 
        let rnd = new System.Random(34545)
        imageData <- 
            Array.init 
                (self.ImageSize * self.ImageSize * 2) 
                (fun _ -> byte (rnd.Next(256)))

    [<Benchmark>]
    member self.FilterScanlinesOld() = 
        filterScanlines 
            self.ImageSize self.ImageSize 16 imageData useBestFilterForScanline

    [<Benchmark>]
    member self.FilterScanlinesWithSeparateFirstLineHandling() = 
        filterScanlinesWithFirstLineSeparated 
            self.ImageSize self.ImageSize 16 imageData
