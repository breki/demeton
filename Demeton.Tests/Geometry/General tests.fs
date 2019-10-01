module Geometry.``General tests``

open Demeton.Geometry

open Xunit
open Swensen.Unquote
open TestHelp

[<Fact>]
let ``Splits coords into two lists``() =
    test <@ 
            [ 10.; 20.; 30.; 40. ] |> splitCoords = ([10.; 30.], [20.; 40.])
         @>

[<Fact>]
let ``Constructs a list of points from a coords list``() =
    test <@ 
            [ 10.; 20.; 30.; 40. ] |> floatsListToPoints 
                = ([(10., 20.); (30., 40.)])
         @>

[<Fact>]
let ``Returns Ok if all points are inside the box``() =
    let points = 
        [(10., 20.); (11., 21.); (12., 22.)]

    test <@ areAllPointsInsideBox 5. 7. 50. 60. points @>


[<Theory>]
[<InlineData(100., 20.)>]
[<InlineData(15., 100.)>]
[<InlineData(15., -100.)>]
[<InlineData(-100, 20.)>]
let ``Returns an error if some points are outside of the box``
    (offendingX, offendingY) =
    let points = 
        [(10., 20.); (11., 21.); (offendingX, offendingY); (12., 22.)]

    test <@ not (areAllPointsInsideBox 5. 7. 50. 60. points) @>
