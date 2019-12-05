module Tests.``Vectorization tests``.``Marching squares tests``

open Demeton.Vectorization.MarchingSquares
open Tests.``Vectorization tests``.``Isoline DSL``
open Xunit
open Swensen.Unquote

let getHeight (array: float[]) width height x y =
    match x, y with
    | _ when x < 0 -> raise (System.ArgumentOutOfRangeException "x") 
    | _ when x >= width -> raise (System.ArgumentOutOfRangeException "x") 
    | _ when y < 0 -> raise (System.ArgumentOutOfRangeException "y")
    | _ when y >= height -> raise (System.ArgumentOutOfRangeException "y")
    | _ -> array.[y * width + x]

let segmentationFunc array width height isoValue: SegmentationFunc =
    fun isolinePoint ->
    match isolinePoint with
    | HPoint (OnHorizontalEdge (x, y)) ->
        let hUp = getHeight array width height x y
        let hDown = getHeight array width height x (y + 1)
        match hUp, isoValue, hDown with
        | _ when hUp <= isoValue && isoValue < hDown ->
            HStep (OnHorizontalEdge (x, y), Right) |> Some
        | _ when hUp > isoValue && isoValue >= hDown ->
            HStep (OnHorizontalEdge (x, y), Left) |> Some
        | _ -> None
    | VPoint (OnVerticalEdge (x, y)) ->
        let hLeft = getHeight array width height x y
        let hRight = getHeight array width height (x + 1) y
        match hLeft, isoValue, hRight with
        | _ when hLeft <= isoValue && isoValue < hRight ->
            VStep (OnVerticalEdge (x, y), Up) |> Some
        | _ when hLeft > isoValue && isoValue >= hRight ->
            VStep (OnVerticalEdge (x, y), Down) |> Some
        | _ -> None


[<Fact>]
let ``Very small array``() =
    let width = 1
    let height = 2
    let testArray = [|
        5.
        10.
    |]

    let isolines =
        findIsolines width height (segmentationFunc testArray width height 5.)
        |> Seq.toList
    
    test <@ isolines = [ ClippedIsoline {
                Steps = [
                    HStep (OnHorizontalEdge (0, 0), Right)
                ] } ]
            @>

[<Fact>]
let ``Simple peak``() =
    let width = 3
    let height = 3
    let testArray = [|
        0.; 0.; 0.
        0.; 100.; 0.
        0.; 0.; 0.
    |]

    let isolines =
        findIsolines width height (segmentationFunc testArray width height 50.)
        |> Seq.toList
    
    test <@ isolines = [ ClosedIsoline {
                Steps = [
                    HStep (OnHorizontalEdge (1, 0), Right)
                    VStep (OnVerticalEdge (1, 1), Down)
                    HStep (OnHorizontalEdge (1, 1), Left)
                    VStep (OnVerticalEdge (0, 1), Up)
                ] } ]
            @>

[<Fact>]
let ``Simple hole``() =
    let width = 3
    let height = 3
    let testArray = [|
        100.; 100.; 100.
        100.; 0.; 100.;
        100.; 100.; 100.
    |]

    let isolines =
        findIsolines width height 
            (segmentationFunc testArray width height 50.)

        |> Seq.toList
    
    test <@ isolines = [ parseIsolineDef "o;v0,1;drul" ] @>


[<Fact>]
let ``Simple horizontal line (right)``() =
    let width = 3
    let height = 3
    let testArray = [|
        0.; 0.; 0.
        100.; 100.; 100.
        100.; 100.; 100.
    |]

    let isolines =
        findIsolines width height
            (segmentationFunc testArray width height 50.)
        |> Seq.toList
    
    test <@ isolines = [ ClippedIsoline {
                Steps = [
                    HStep (OnHorizontalEdge (0, 0), Right)
                    HStep (OnHorizontalEdge (1, 0), Right)
                    HStep (OnHorizontalEdge (2, 0), Right)
                ]
            } ] @>

[<Fact>]
let ``Simple horizontal line (left)``() =
    let width = 3
    let height = 3
    let testArray = [|
        100.; 100.; 100.
        0.; 0.; 0.
        0.; 0.; 0.
    |]

    let isolines =
        findIsolines width height
            (segmentationFunc testArray width height 50.)
        |> Seq.toList
    
    test <@ isolines = [ ClippedIsoline {
                Steps = [
                    HStep (OnHorizontalEdge (2, 0), Left)
                    HStep (OnHorizontalEdge (1, 0), Left)
                    HStep (OnHorizontalEdge (0, 0), Left)
                ]
            } ] @>

[<Fact>]
let ``Simple vertical line (up)``() =
    let width = 3
    let height = 3
    let testArray = [|
        0.; 100.; 100.
        0.; 100.; 100.
        0.; 100.; 100.
    |]

    let isolines =
        findIsolines width height
            (segmentationFunc testArray width height 50.)
        |> Seq.toList
    
    test <@ isolines = [ ClippedIsoline {
                Steps = [
                    VStep (OnVerticalEdge (0, 2), Up)
                    VStep (OnVerticalEdge (0, 1), Up)
                    VStep (OnVerticalEdge (0, 0), Up)
                ]
            } ] @>

[<Fact>]
let ``Simple vertical line (down)``() =
    let width = 3
    let height = 3
    let testArray = [|
        100.; 0.; 0.
        100.; 0.; 0.
        100.; 0.; 0.
    |]

    let isolines =
        findIsolines width height
            (segmentationFunc testArray width height 50.)
        |> Seq.toList
    
    test <@ isolines = [ ClippedIsoline {
                Steps = [
                    VStep (OnVerticalEdge (0, 0), Down)
                    VStep (OnVerticalEdge (0, 1), Down)
                    VStep (OnVerticalEdge (0, 2), Down)
                ]
            } ] @>

[<Fact>]
let ``Simple bend``() =
    let width = 2
    let height = 2
    let testArray = [|
        100.; 0.
        0.; 0.
    |]

    let isolines =
        findIsolines width height
            (segmentationFunc testArray width height 50.)
        |> Seq.toList
    
    test <@ isolines = [ ClippedIsoline {
                Steps = [
                    VStep (OnVerticalEdge (0, 0), Down)
                    HStep (OnHorizontalEdge (0, 0), Left)
                ]
            } ] @>


[<Fact>]
let ``Can handle multiple possible directions case``() =
    let width = 2
    let height = 2
    let testArray = [|
        100.; 0.
        0.; 100.
    |]

    let isolines =
        findIsolines width height
            (segmentationFunc testArray width height 50.)
        |> Seq.toList
    
    test <@ isolines = [
                ClippedIsoline { Steps = [
                    VStep (OnVerticalEdge (0, 0), Down)
                    HStep (OnHorizontalEdge (0, 0), Left)
                ] }
                ClippedIsoline { Steps = [
                    VStep (OnVerticalEdge (0, 1), Up)
                    HStep (OnHorizontalEdge (1, 0), Right)
                ] }
            ] @>

[<Fact>]
let ``Can identify multiple isolines``() =
    let width = 3
    let height = 3
    let testArray = [|
        100.; 0.; 100.
        100.; 0.; 100.
        100.; 0.; 100.
    |]

    let isolines =
        findIsolines width height
            (segmentationFunc testArray width height 50.)
        |> Seq.toList
    
    test <@ isolines = [
                ClippedIsoline { Steps = [
                    VStep (OnVerticalEdge (0, 0), Down)
                    VStep (OnVerticalEdge (0, 1), Down)
                    VStep (OnVerticalEdge (0, 2), Down)
                ] }
                ClippedIsoline { Steps = [
                    VStep (OnVerticalEdge (1, 2), Up)
                    VStep (OnVerticalEdge (1, 1), Up)
                    VStep (OnVerticalEdge (1, 0), Up)
                ] }
            ] @>

[<Fact>]
let ``More complex case``() =
    let width = 3
    let height = 4
    let testArray = [|
        0.; 100.; 0.
        100.; 100.; 0.
        100.; 0.; 100.
        0.; 100.; 0.
    |]

    let isolines =
        findIsolines width height
            (segmentationFunc testArray width height 50.)
        |> Seq.toList
    
    test <@ isolines = [
                ClippedIsoline { Steps = [
                    HStep (OnHorizontalEdge (0, 0), Right)
                    VStep (OnVerticalEdge (0, 0), Up)
                ] }
                ClippedIsoline { Steps = [
                    HStep (OnHorizontalEdge (2, 2), Left);
                    VStep (OnVerticalEdge (1, 2), Up);
                    HStep (OnHorizontalEdge (2, 1), Right)
                ] }
                ClippedIsoline { Steps = [
                    VStep (OnVerticalEdge (1, 0), Down);
                    VStep (OnVerticalEdge (1, 1), Down);
                    HStep (OnHorizontalEdge (1, 1), Left);
                    VStep (OnVerticalEdge (0, 2), Down);
                    HStep (OnHorizontalEdge (0, 2), Left)
                ] }
                ClippedIsoline { Steps = [
                    VStep (OnVerticalEdge (0, 3), Up);
                    HStep (OnHorizontalEdge (1, 2), Right);
                    VStep (OnVerticalEdge (1, 3), Down)
                ] }
            ] @>

[<Fact>]
let ``Can detect isolines clipping from bottom right``() =
    let width = 2
    let height = 4
    let testArray = [|
        7.; 3.
        4.; 10.
        1.; 0.
        0.; 10.
    |]

    let isolines =
        findIsolines width height
            (segmentationFunc testArray width height 6.)
        |> Seq.toList
    
    test <@ isolines = [
                ClippedIsoline { Steps =
                    [VStep (OnVerticalEdge (0, 0), Down);
                     HStep (OnHorizontalEdge (0, 0), Left)] };
                ClippedIsoline { Steps =
                    [HStep (OnHorizontalEdge (1, 1), Left);
                     VStep (OnVerticalEdge (0, 1), Up);
                    HStep (OnHorizontalEdge (1, 0), Right)] };
                ClippedIsoline { Steps =
                    [VStep (OnVerticalEdge (0, 3), Up);
                     HStep (OnHorizontalEdge (1, 2), Right)] }] @>
  


