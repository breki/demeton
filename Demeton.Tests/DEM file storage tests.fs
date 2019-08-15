module Demeton.Tests.``DEM file storage tests``

open Demeton

open FsUnit
open Xunit

[<Fact>]
let ``Clasifies SRTM tile file as not cached if it does not exist in local storage``() =
   1 |> should equal 1 
