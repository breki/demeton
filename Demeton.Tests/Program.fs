module Demeton.Tests.DummyConsole

open BenchmarkDotNet.Running
open Demeton.Benchmarks.ArrayAccess
open Demeton.Benchmarks.Array1Dvs2D
open Demeton.Benchmarks.CrcBenchmarks
open Demeton.Benchmarks.PngFilterBenchmarks

let defaultSwitch () = 
    BenchmarkSwitcher 
        [| 
            typeof<ArrayAccessComparison>;
            typeof<Array1Dvs2DComparison>;
            typeof<Crc32Comparison>;
            typeof<PngFilterComparison>
        |]

[<EntryPoint>]
let main args =
    let summary = defaultSwitch().Run args
    0

