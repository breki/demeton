module Tests.``Vectorization tests``.``Steps to moves transformation``

open Demeton.Vectorization.MarchingSquares
open Tests.``Vectorization tests``.SampleSegmentation
open Tests.``Vectorization tests``.``Isoline DSL``

open Xunit
open Swensen.Unquote
open FsCheck

type MoveDirection = 
    | N  | NE | E | SE | S | SW | W | NW

type Move = {
    Direction: MoveDirection
    Count: int 
}

let moveN by = { Direction = N; Count = by }
let moveNE by = { Direction = NE; Count = by }
let moveE by = { Direction = E; Count = by }
let moveSE by = { Direction = SE; Count = by }
let moveS by = { Direction = S; Count = by }
let moveSW by = { Direction = SW; Count = by }
let moveW by = { Direction = W; Count = by }
let moveNW by = { Direction = NW; Count = by }

type IsolineMoves = {
    StartingPoint: Demeton.Geometry.Common.Point
    Moves: Move list
}

let stepsToMoves (isoline: Isoline): IsolineMoves =
    let processNextStep (previousStepMaybe, directionsSoFar) stepMaybe =
        match previousStepMaybe, directionsSoFar, stepMaybe with
        | None, [], Some firstStep ->
            match isoline with
            | ClippedIsoline _ ->
                let initialDirection =
                    match firstStep with
                    | HStep (_, Left) -> W
                    | HStep (_, Right) -> E
                    | VStep (_, Up) -> N
                    | VStep (_, Down) -> S
                (Some firstStep, [initialDirection])
            | ClosedIsoline _ -> invalidOp "todo"
        | Some previousStep, _, Some nextStep ->
            let nextDirection =
                match previousStep, nextStep with
                | HStep(_, Left), HStep(_, Left) -> W
                | HStep(_, Left), VStep(_, Up) -> NW
                | HStep(_, Left), VStep(_, Down) -> SW
                | HStep(_, Right), HStep(_, Right) -> E 
                | HStep(_, Right), VStep(_, Up) -> NE
                | HStep(_, Right), VStep(_, Down) -> SE
                | VStep(_, Up), VStep(_, Up) -> N
                | VStep(_, Up), HStep(_, Left) -> NW
                | VStep(_, Up), HStep(_, Right) -> NE
                | VStep(_, Down), VStep(_, Down) -> S
                | VStep(_, Down), HStep(_, Left) -> SW
                | VStep(_, Down), HStep(_, Right) -> SE
                | _ -> invalidOp "bug: invalid step sequence"
            (Some nextStep, nextDirection :: directionsSoFar)
        | Some lastStep, _, None ->
            match isoline with
            | ClippedIsoline _ ->
                let lastDirection = 
                    match lastStep with
                    | HStep (_, Left) ->W
                    | HStep (_, Right) ->E
                    | VStep (_, Up) -> N
                    | VStep (_, Down) -> S
                (None, lastDirection :: directionsSoFar)
            | ClosedIsoline _ -> invalidOp "todo"       
        | _ -> invalidOp "bug: this should never happen"
    
    let steps = isolineSteps isoline

    let (_, directions) =
        steps
        |> List.rev
        |> List.map Option.Some
        |> List.append [ None ]
        |> List.rev
        |> List.fold processNextStep (None, [])
    
    let directionsToMoves
        (movesSoFar, prevDirectionMaybe, prevMoveRepeatCount) directionMaybe =
        match movesSoFar, prevDirectionMaybe, directionMaybe with
        | [], None, Some direction -> ([], Some direction, 1)
        | _, Some prevDirection, Some direction ->  
            if direction = prevDirection then
                (movesSoFar, Some prevDirection, prevMoveRepeatCount + 1)
            else
                let moveToAdd =
                    { Direction = prevDirection; Count = prevMoveRepeatCount }
                (moveToAdd :: movesSoFar, Some direction, 1)
        | _, Some prevDirection, None ->  
            let moveToAdd =
                { Direction = prevDirection; Count = prevMoveRepeatCount }
            (moveToAdd :: movesSoFar, None, 0)
        | _ -> invalidOp "bug: this should never happen"
                
    
    let (completeMoves, _, _) =
        directions
        |> List.map Option.Some
        |> List.append [ None ]
        |> List.rev
        |> List.fold directionsToMoves ([], None, 0)
    
    let startingPoint =
        match isoline with
        | ClippedIsoline _ ->
            match steps.[0] with
            | HStep (OnHorizontalEdge (x0, y0), Left) ->
                ((x0 |> float) + 0.5, (y0 |> float) + 0.5)
            | HStep (OnHorizontalEdge (x0, y0), Right) ->
                ((x0 |> float) - 0.5, (y0 |> float) + 0.5)
            | VStep (OnVerticalEdge (x0, y0), Up) ->
                ((x0 |> float) + 0.5, (y0 |> float) + 0.5)
            | VStep (OnVerticalEdge (x0, y0), Down) ->
                ((x0 |> float) + 0.5, (y0 |> float) - 0.5)
        | ClosedIsoline _ -> invalidOp "todo"
        
    { StartingPoint = startingPoint; Moves = completeMoves |> List.rev }

let movesToSteps (moves: IsolineMoves): Isoline =
    let (sxFloat, syFloat) = moves.StartingPoint
    let sx = sxFloat |> int
    let sy = syFloat |> int
    
    match moves.Moves.[0] with
    | { Direction = W; Count = 1} -> ClosedIsoline {
        Steps = [ HStep (OnHorizontalEdge (sx, sy), Left) ] }
    | { Direction = E; Count = 1} -> ClosedIsoline {
        Steps = [ HStep (OnHorizontalEdge (sx, sy), Right) ] }
    | _ ->  ClosedIsoline { Steps = [] }

[<Fact(Skip="todo")>]    
//[<Fact>]
let ``Simple isoline left``() =
    let isoline = parseIsolineDef "l;h0,0;l"
    
    test <@ stepsToMoves isoline =
                { StartingPoint = (0.5, 0.5); Moves = [ moveW 2 ] } @>
    test <@ isoline |> stepsToMoves |> movesToSteps = isoline @>

[<Fact>]
let ``Simple isoline right``() =
    let isoline = parseIsolineDef "l;h0,0;r"
    
    test <@ stepsToMoves isoline =
                { StartingPoint = (-0.5, 0.5); Moves = [ moveE 2 ] } @>

[<Fact>]
let ``Two steps isoline``() =
    let isoline = parseIsolineDef "l;h1,0;ld"
    
    test <@ stepsToMoves isoline =
                { StartingPoint = (1.5, 0.5);
                  Moves = [ moveW 1; moveSW 1; moveS 1 ] } @>

let ``isoline moves properties``((heightsArray, isolineHeight): int[,] * int) =
    let width = heightsArray |> Array2D.length1
    let height = heightsArray |> Array2D.length2

    match width, height with
    | (0, _) -> true |> Prop.classify true "Empty array"
    | (_, 0) -> true |> Prop.classify true "Empty array"
    | _ ->
        let offendingIsolines =
            findIsolines
                width height (heightsSegmentation heightsArray isolineHeight)
            |> Seq.toArray
            |> Array.map (fun isoline ->
                let moves = stepsToMoves isoline
                let isolineBack = movesToSteps moves
                (isoline, moves, isolineBack))
            |> Array.filter (fun (isoline, _, isolineBack) ->
                isoline <> isolineBack)
        
        offendingIsolines |> Array.isEmpty
        |> Prop.label
               "transformation from steps to moves and back produces the same steps"
        |@ sprintf "Offending isolines: %A" offendingIsolines
            
[<Fact(Skip="todo")>]    
//[<Fact>]    
let ``Test isoline moves properties``() =
    let genHeight = Gen.choose(0, 10)
    let genArray = genHeight |> Gen.array2DOf

    Gen.zip genArray genHeight 
    |> Arb.fromGen
    |> Prop.forAll <| ``isoline moves properties``
    |> Check.QuickThrowOnFailure
//    |> replayPropertyCheck (1567451850,296676651) // not at the edge
