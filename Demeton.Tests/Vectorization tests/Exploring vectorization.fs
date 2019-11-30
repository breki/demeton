module Tests.``Vectorization tests``.``Exploring vectorization``

open Xunit
open Swensen.Unquote

type IsolinePoint =
    | OnHorizontalEdge of (int * int)
    | OnVerticalEdge of (int * int)

type IsolineStep =
    | Up
    | Right
    | Down
    | Left

type IsolineMovement = IsolineStep list

type Isoline = {
    StartingPoint: IsolinePoint
    Movement: IsolineMovement
}

let findIsoline
    width height (heights: int -> int -> float) isolineValue
    : Isoline option =
    let tryFindNextStartingPoint(): IsolinePoint option =
        let xs = seq { 0 .. width-2 }
        let ys = seq { 0 .. height-2 }
    
        Seq.allPairs xs ys
        |> Seq.tryPick (fun (x, y) ->
            let h00 = heights x y
            let h10 = heights (x + 1) y
            if isolineValue > h00 && isolineValue < h10 then
                OnVerticalEdge (x, y) |> Some
            else
                let h01 = heights x (y + 1)
                if isolineValue > h00 && isolineValue < h01 then
                    OnHorizontalEdge (x, y) |> Some
                else
                    None
        )
    
    let moveIsolinePoint fromPoint step =
        invalidOp "todo"
    
    let traceIsoline startingPoint =
        match startingPoint with
        | OnVerticalEdge (x, y) ->
            let nextStep = Up
            { StartingPoint = startingPoint; Movement = [ nextStep ] }
        | OnHorizontalEdge (x, y) -> invalidOp "todo"
    
    tryFindNextStartingPoint()
    |> Option.map traceIsoline

[<Fact>]
let ``Simple case``() =
    let width = 3
    let height = 3
    let testArray = [|
        0.; 0.; 0.
        0.; 100.; 0.
        0.; 0.; 0.
    |]

    let heights x y = testArray.[y * height + x]

    let isoline = findIsoline width height heights 50.
    
    test <@ isoline = Some {
                StartingPoint = OnVerticalEdge (0, 1)
                Movement = [ Up ]
            } @>
