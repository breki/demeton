module Demeton.Tests.DummyConsole

open BenchmarkDotNet.Running
open Demeton.Benchmarks.ArrayAccess
open Demeton.Benchmarks.Array1Dvs2D

let defaultSwitch () = 
    BenchmarkSwitcher 
        [| 
            typeof<ArrayAccessComparison>;
            typeof<Array1Dvs2DComparison> 
        |]

[<EntryPoint>]
let main args =
    let summary = defaultSwitch().Run args
    0

