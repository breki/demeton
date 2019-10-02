module Geometry.``Bounds tests``

open Demeton.Geometry
open Demeton.Geometry.Common

open Xunit
open Swensen.Unquote

let bounds = { MinX = 10.; MinY = 20.; MaxX = 30.; MaxY = 40.}

[<Fact>]
let ``isInsideInclusive returns true if point is inside and not touching the boundary``() =
    test <@ bounds |> Bounds.isInsideInclusive 15. 35. @>

[<Fact>]
let ``Calculates minimum bounding rectangle for a series of points``() =
    let points = 
        [(10., 20.); (11., 21.); (12., 22.)]

    test <@ Bounds.mbrOf points 
        = { MinX = 10.; MinY = 20.; MaxX = 12.; MaxY = 22. } @>
