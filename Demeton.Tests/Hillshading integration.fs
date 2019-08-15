module Demeton.Tests.``Hillshading integration``

open Demeton

open FsUnit
open Xunit

[<Fact(Skip="to be implemented")>]
let ``Returns a stream of PNG file containing hillshading of a specified region``() =
    // 15.297432,46.401552,15.67131,46.577648
    let stream = hillshade { 
        MinLon = 15.297432; 
        MinLat = 46.401552; 
        MaxLon = 15.67131; 
        MaxLat = 46.577648 }
    stream |> should not' (equal None)
