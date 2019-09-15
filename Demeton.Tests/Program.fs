module Demeton.Tests.DummyConsole

open BenchmarkDotNet.Running
open Demeton.BenchmarkingTests

let defaultSwitch () = 
    BenchmarkSwitcher [| typeof<ArrayAccessComparison> |]

// Added this because VS keeps switching the test project from a class library
// to a console and then giving me warnings about the missing entry point.
[<EntryPoint>]
let main args =
    let summary = defaultSwitch().Run args
    0

