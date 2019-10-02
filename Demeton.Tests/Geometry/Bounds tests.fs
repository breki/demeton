module Geometry.``Bounds tests``

open Demeton.Geometry
open Demeton.Geometry.Common
open GeometryTestHelp

open Xunit
open Swensen.Unquote

let bounds = { MinX = 10.; MinY = 20.; MaxX = 30.; MaxY = 40.}

[<Fact>]
let ``isInsideInclusive returns true if point is inside and not touching the boundary``() =
    test <@ bounds |> Bounds.isInsideInclusive 15. 35. @>

[<Theory>]
[<InlineData(10., 25.)>]
[<InlineData(15., 20.)>]
[<InlineData(30., 25.)>]
[<InlineData(15., 40.)>]
let ``isInsideInclusive returns true if point is touching the boundary`` x y =
    test <@ bounds |> Bounds.isInsideInclusive x y @>

[<Theory>]
[<InlineData(15., 45.)>]
[<InlineData(31., 30.)>]
[<InlineData(31., 45.)>]
let ``isInsideInclusive returns false if point is outside`` x y =
    test <@ bounds |> Bounds.isInsideInclusive x y |> not @>

[<Theory>]
[<InlineData(10., 25.)>]
[<InlineData(15., 25.)>]
let ``extendWith leaves the same bounds if the point to extend with is inside``
    x y =
    test <@ bounds |> Bounds.extendWith x y = bounds @>

[<Fact>]
let ``Calculates minimum bounding rectangle for a series of points``() =
    let points = 
        [(10., 20.); (11., 21.); (12., 22.)]

    test <@ Bounds.mbrOf points 
        = { MinX = 10.; MinY = 20.; MaxX = 12.; MaxY = 22. } @>

[<RandomPointsProperty>]
let ``All points are inside of their MBR`` (points: RandomPoints) =
    let mbr = points |> Bounds.mbrOf
    points 
    |> Seq.exists (fun (x, y) -> Bounds.isInsideInclusive x y mbr |> not) 
    |> not

[<RandomPointsProperty>]
let ``All MBR edges are touched by at least one point`` (points: RandomPoints) =
    match points.Length with
    | 0 -> true
    | _ -> 
        let mbr = points |> Bounds.mbrOf
        Seq.exists (fun (x, _) -> x = mbr.MinX) points
            && Seq.exists (fun (x, _) -> x = mbr.MaxX) points 
            && Seq.exists (fun (_, y) -> y = mbr.MinY) points 
            && Seq.exists (fun (_, y) -> y = mbr.MaxY) points 

[<RandomPointsProperty>]
let ``extendWith results in a MBR of the existing bounds and the new point``
    ((x, y): RandomPoint) =
    let bounds = { MinX = -50.0; MinY = -50.0; MaxX = 50.0; MaxY = 50. }

    bounds |> Bounds.extendWith x y 
        = Bounds.mbrOf [(-50., -50.); (50., 50.); (x, y)]
