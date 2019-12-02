module Tests.``Vectorization tests``.``Isoline tests``

open Demeton.Vectorization.Isolines
open Xunit
open Swensen.Unquote

let getHeight (array: float[]) width height x y =
    match x, y with
    | _ when x < 0 -> raise (System.ArgumentOutOfRangeException "x") 
    | _ when x >= width -> raise (System.ArgumentOutOfRangeException "x") 
    | _ when y < 0 -> raise (System.ArgumentOutOfRangeException "y")
    | _ when y >= height -> raise (System.ArgumentOutOfRangeException "y")
    | _ -> array.[y * width + x]

[<Fact>]
let ``Very small array``() =
    let width = 1
    let height = 2
    let testArray = [|
        5.
        10.
    |]

    let heights = getHeight testArray width height

    let isolines =
        findIsolines width height heights 5.
        |> Seq.toList
    
    test <@ isolines = [ ClippedIsoline {
                Steps = [
                    (OnHorizontalEdge (0, 0), Right)
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

    let heights = getHeight testArray width height

    let isolines =
        findIsolines width height heights 50.
        |> Seq.toList
    
    test <@ isolines = [ ClosedIsoline {
                Steps = [
                    (OnHorizontalEdge (1, 0), Right)
                    (OnVerticalEdge (1, 1), Down)
                    (OnHorizontalEdge (1, 1), Left)
                    (OnVerticalEdge (0, 1), Up)
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

    let heights = getHeight testArray width height

    let isolines =
        findIsolines width height heights 50.
        |> Seq.toList
    
    test <@ isolines = [ ClosedIsoline {
                Steps = [
                    OnVerticalEdge (0, 1), Down
                    OnHorizontalEdge (1, 1), Right
                    OnVerticalEdge (1, 1), Up
                    OnHorizontalEdge (1, 0), Left
                ]
            } ] @>


[<Fact>]
let ``Simple horizontal line (right)``() =
    let width = 3
    let height = 3
    let testArray = [|
        0.; 0.; 0.
        100.; 100.; 100.
        100.; 100.; 100.
    |]

    let heights = getHeight testArray width height

    let isolines =
        findIsolines width height heights 50.
        |> Seq.toList
    
    test <@ isolines = [ ClippedIsoline {
                Steps = [
                    OnHorizontalEdge (0, 0), Right
                    OnHorizontalEdge (1, 0), Right
                    OnHorizontalEdge (2, 0), Right
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

    let heights = getHeight testArray width height

    let isolines =
        findIsolines width height heights 50.
        |> Seq.toList
    
    test <@ isolines = [ ClippedIsoline {
                Steps = [
                    OnHorizontalEdge (2, 0), Left
                    OnHorizontalEdge (1, 0), Left
                    OnHorizontalEdge (0, 0), Left
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

    let heights = getHeight testArray width height

    let isolines =
        findIsolines width height heights 50.
        |> Seq.toList
    
    test <@ isolines = [ ClippedIsoline {
                Steps = [
                    OnVerticalEdge (0, 2), Up
                    OnVerticalEdge (0, 1), Up
                    OnVerticalEdge (0, 0), Up
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

    let heights = getHeight testArray width height

    let isolines =
        findIsolines width height heights 50.
        |> Seq.toList
    
    test <@ isolines = [ ClippedIsoline {
                Steps = [
                    OnVerticalEdge (0, 0), Down
                    OnVerticalEdge (0, 1), Down
                    OnVerticalEdge (0, 2), Down
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

    let heights = getHeight testArray width height

    let isolines =
        findIsolines width height heights 50.
        |> Seq.toList
    
    test <@ isolines = [ ClippedIsoline {
                Steps = [
                    OnVerticalEdge (0, 0), Down
                    OnHorizontalEdge (0, 0), Left
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

    let heights = getHeight testArray width height

    let isolines =
        findIsolines width height heights 50.
        |> Seq.toList
    
    test <@ isolines = [
                ClippedIsoline { Steps = [
                    OnVerticalEdge (0, 0), Down
                    OnHorizontalEdge (0, 0), Left
                ] }
                ClippedIsoline { Steps = [
                    OnVerticalEdge (0, 1), Up
                    OnHorizontalEdge (1, 0), Right
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

    let heights = getHeight testArray width height

    let isolines =
        findIsolines width height heights 50.
        |> Seq.toList
    
    test <@ isolines = [
                ClippedIsoline { Steps = [
                    OnVerticalEdge (0, 0), Down
                    OnVerticalEdge (0, 1), Down
                    OnVerticalEdge (0, 2), Down
                ] }
                ClippedIsoline { Steps = [
                    OnVerticalEdge (1, 2), Up
                    OnVerticalEdge (1, 1), Up
                    OnVerticalEdge (1, 0), Up
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

    let heights = getHeight testArray width height

    let isolines =
        findIsolines width height heights 50.
        |> Seq.toList
    
    test <@ isolines = [
                ClippedIsoline { Steps = [
                    OnHorizontalEdge (0, 0), Right
                    OnVerticalEdge (0, 0), Up
                ] }
                ClippedIsoline { Steps = [
                    (OnHorizontalEdge (2, 2), Left);
                    (OnVerticalEdge (1, 2), Up);
                    (OnHorizontalEdge (2, 1), Right)
                ] }
                ClippedIsoline { Steps = [
                    (OnVerticalEdge (1, 0), Down);
                    (OnVerticalEdge (1, 1), Down);
                    (OnHorizontalEdge (1, 1), Left);
                    (OnVerticalEdge (0, 2), Down);
                    (OnHorizontalEdge (0, 2), Left)
                ] }
                ClippedIsoline { Steps = [
                    (OnVerticalEdge (0, 3), Up);
                    (OnHorizontalEdge (1, 2), Right);
                    (OnVerticalEdge (1, 3), Down)
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

    let heights = getHeight testArray width height

    let isolines =
        findIsolines width height heights 6.
        |> Seq.toList
    
    test <@ isolines = [
                ClippedIsoline { Steps =
                    [(OnVerticalEdge (0, 0), Down);
                     (OnHorizontalEdge (0, 0), Left)] };
                ClippedIsoline { Steps =
                    [(OnHorizontalEdge (1, 1), Left);
                     (OnVerticalEdge (0, 1), Up);
                    (OnHorizontalEdge (1, 0), Right)] };
                ClippedIsoline { Steps =
                    [(OnVerticalEdge (0, 3), Up);
                     (OnHorizontalEdge (1, 2), Right)] }] @>
  


