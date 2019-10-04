module ``Commands tests``.``ShadeCommand``.``Splitting into intervals``

open Demeton.Commands

open Xunit
open Swensen.Unquote

[<Fact>]
let ``Correctly splits into intervals when all intervals will have the same size``() =
    test <@ ShadeCommand.splitIntoIntervals 10 100 30 |> Seq.toList
                = [ (0, 10, 40); (1, 40, 70); (2, 70, 100) ]
        @>

[<Fact>]
let ``Correctly splits into intervals when the last interval will be smaller``() =
    test <@ ShadeCommand.splitIntoIntervals 10 90 30 |> Seq.toList
                = [ (0, 10, 40); (1, 40, 70); (2, 70, 90) ]
        @>

[<Fact>]
let ``Correctly splits into a single interval``() =
    test <@ ShadeCommand.splitIntoIntervals 10 90 100 |> Seq.toList
                = [ (0, 10, 90) ]
        @>

