module Demeton.Benchmarks.Array1Dvs2D

open BenchmarkDotNet.Attributes
open System

type Array1Dvs2DComparison() = 

    [<Params (1000, 100000)>] 
    member val public ArraySize = 0 with get, set

    [<Benchmark>]
    member self.Access1DArray() =
        let rnd = new System.Random(234)
        let width = int (sqrt (float self.ArraySize))
        let height = width

        let array = 
            Array.init self.ArraySize (fun i -> byte (rnd.Next(255)))

        let mutable sum = 0
        for y in 0 .. (height-1) do
            for x in 0 .. (width-1) do
                let value = array.[x + (y * width)]
                sum <- sum + int value

    [<Benchmark>]
    member self.Access2DArray() =
        let rnd = new System.Random(234)

        let width = int (sqrt (float self.ArraySize))
        let height = width

        let array = 
            Array2D.init width height (fun _ _ -> byte (rnd.Next(255)))

        let mutable sum = 0
        for y in 0 .. (height-1) do
            for x in 0 .. (width-1) do
                let value = array.[x, y]
                sum <- sum + int value
