module Demeton.Tests.``Reading HGT files``

open Demeton

open FsUnit
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Merging empty DEM data array results in None``() =
    Dem.merge ([]) |> should equal None
