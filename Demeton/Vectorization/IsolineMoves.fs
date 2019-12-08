module Demeton.Vectorization.IsolineMoves

open Demeton.Vectorization.MarchingSquares

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

type ClosedIsolineMoves = {
    StartingPoint2: (int * int)
    Moves: Move list
}

type ClippingEdge =
    | TopEdge of int
    | RightEdge of (int * int)
    | BottomEdge of (int * int)
    | LeftEdge of int

type ClippedIsolineMoves = {
    StartingEdge: ClippingEdge
    EndingEdge: ClippingEdge
    Moves: Move list
}

type IsolineMoves =
    | ClosedIsolineMoves of ClosedIsolineMoves
    | ClippedIsolineMoves of ClippedIsolineMoves

let private directionsToMoves
    (movesSoFar, prevDirectionMaybe, prevMoveRepeatCount) directionMaybe =
    match movesSoFar, prevDirectionMaybe, directionMaybe with
    | [], None, None -> ([], None, 0)
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

    
let private findNextMoveDirection previousStepMaybe directionsSoFar nextStep =
    match previousStepMaybe, directionsSoFar with
    | None, [] -> (Some nextStep, [])
    | Some previousStep, _ ->
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
    | _ -> invalidOp "bug: this should never happen"

let private closedStepsToMoves (isoline: ClosedIsoline): IsolineMoves =
    let processNextStep (previousStepMaybe, directionsSoFar) nextStepMaybe =
        let nextStep =
            match nextStepMaybe with
            | Some step -> step
            | None -> isoline.Steps |> Seq.head
        
        findNextMoveDirection previousStepMaybe directionsSoFar nextStep
    
    let (_, directions) =
        isoline.Steps
        |> Seq.rev
        |> Seq.map Option.Some
        |> Seq.append [ None ]
        |> Seq.rev
        |> Seq.fold processNextStep (None, [])
    
    let (completeMoves, _, _) =
        directions
        |> Seq.map Option.Some
        |> Seq.append [ None ]
        |> Seq.rev
        |> Seq.fold directionsToMoves ([], None, 0)
    
    let startingPoint =
        match isoline.Steps.Head with
        | HStep (OnHorizontalEdge (x, y), _) -> (x * 2, y * 2 + 1)
        | VStep (OnVerticalEdge (x, y), _) -> (x * 2 + 1, y * 2)
        
    ClosedIsolineMoves {
        StartingPoint2 = startingPoint; Moves = completeMoves |> List.rev
    }
        
let private clippedStepsToMoves (isoline: ClippedIsoline): IsolineMoves =
    let processNextStep (previousStepMaybe, directionsSoFar) nextStep =        
        findNextMoveDirection previousStepMaybe directionsSoFar nextStep
    
    let startingEdge =
        match isoline.Steps |> Seq.head with
        | HStep (OnHorizontalEdge (x, y), Left) -> RightEdge (x * 2, y * 2 + 1)
        | HStep (OnHorizontalEdge (_, y), Right) -> LeftEdge (y * 2 + 1)
        | VStep (OnVerticalEdge (x, y), Up) -> BottomEdge (x * 2 + 1, y * 2)
        | VStep (OnVerticalEdge (x, _), Down) -> TopEdge (x * 2 + 1)
    
    let endingEdge =
        match isoline.Steps |> Seq.last with
        | HStep (OnHorizontalEdge (_, y), Left) -> LeftEdge (y * 2 + 1)
        | HStep (OnHorizontalEdge (x, y), Right) -> RightEdge (x * 2, y * 2 + 1)
        | VStep (OnVerticalEdge (x, _), Up) -> TopEdge (x * 2 + 1)
        | VStep (OnVerticalEdge (x, y), Down) -> BottomEdge (x * 2 + 1, y * 2)
    
    let (_, directions) =
        isoline.Steps
        |> Seq.fold processNextStep (None, [])
       
    let (completeMoves, _, _) =
        directions
        |> Seq.map Option.Some
        |> Seq.append [ None ]
        |> Seq.rev
        |> Seq.fold directionsToMoves ([], None, 0)
    
    ClippedIsolineMoves {
        StartingEdge = startingEdge
        EndingEdge = endingEdge
        Moves = completeMoves |> List.rev
    }
    
let stepsToMoves (isoline: Isoline): IsolineMoves =
    match isoline with
    | ClosedIsoline closedIsoline -> closedStepsToMoves closedIsoline
    | ClippedIsoline clippedIsoline -> clippedStepsToMoves clippedIsoline

let private movesToDirections (moves: Move seq): MoveDirection seq =
    moves
    |> Seq.collect (fun move -> Seq.init move.Count (fun _ -> move.Direction))

let private processDirection (stepsSoFar, stepPoint: Point) direction =
    let (step, nextStepPoint: Point) =
        match direction, stepPoint with
        | N, VPoint (OnVerticalEdge(x, y)) ->
            (VStep (OnVerticalEdge(x, y), Up), OnVerticalEdge (x, y-1) |> VPoint)
        | NE, VPoint (OnVerticalEdge(x, y)) ->
            (VStep (OnVerticalEdge(x, y), Up), OnHorizontalEdge (x+1, y-1) |> HPoint)
        | NE, HPoint (OnHorizontalEdge(x, y)) ->
            (HStep (OnHorizontalEdge(x, y), Right), OnVerticalEdge (x, y) |> VPoint)
        | E, HPoint (OnHorizontalEdge(x, y)) ->
            (HStep (OnHorizontalEdge(x, y), Right), OnHorizontalEdge (x+1, y) |> HPoint)
        | SE, VPoint (OnVerticalEdge(x, y)) ->
            (VStep (OnVerticalEdge(x, y), Down), OnHorizontalEdge (x+1, y) |> HPoint)
        | SE, HPoint (OnHorizontalEdge(x, y)) ->
            (HStep (OnHorizontalEdge(x, y), Right), OnVerticalEdge (x, y + 1) |> VPoint)
        | S, VPoint (OnVerticalEdge(x, y)) ->
            (VStep (OnVerticalEdge(x, y), Down), OnVerticalEdge (x, y+1) |> VPoint)
        | SW, VPoint (OnVerticalEdge(x, y)) ->
            (VStep (OnVerticalEdge(x, y), Down), OnHorizontalEdge (x, y) |> HPoint)
        | SW, HPoint (OnHorizontalEdge(x, y)) ->
            (HStep (OnHorizontalEdge(x, y), Left), OnVerticalEdge (x-1, y+1) |> VPoint)
        | W, HPoint (OnHorizontalEdge(x, y)) ->
            (HStep (OnHorizontalEdge(x, y), Left), OnHorizontalEdge (x-1, y) |> HPoint)
        | NW, VPoint (OnVerticalEdge(x, y)) ->
            (VStep (OnVerticalEdge(x, y), Up), OnHorizontalEdge (x, y-1) |> HPoint)
        | NW, HPoint (OnHorizontalEdge(x, y)) ->
            (HStep (OnHorizontalEdge(x, y), Left), OnVerticalEdge (x-1, y) |> VPoint)
        | _ -> invalidOp "bug: should never happen"
    
    (step :: stepsSoFar, nextStepPoint)

let private closedMovesToSteps (moves: ClosedIsolineMoves): Isoline =
    let (sx2, sy2) = moves.StartingPoint2
    let sx = sx2 / 2
    let sy = sy2 / 2
        
    let xIsFraction = sx2 % 2 <> 0  
    let yIsFraction = sy2 % 2 <> 0
    
    let startingPoint =
        match xIsFraction, yIsFraction with
        | true, false -> OnVerticalEdge(sx, sy) |> VPoint
        | false, true -> OnHorizontalEdge(sx, sy) |> HPoint
        | _ -> invalidOp "bug: this should not happen"
        
    let (steps, _) =
        moves.Moves
        |> movesToDirections
        |> Seq.fold processDirection ([], startingPoint)
    
    let stepsOrdered = steps |> List.rev
    ClosedIsoline { Steps = stepsOrdered }

let private clippedMovesToSteps (moves: ClippedIsolineMoves): Isoline =
    let startingStepPoint =
        match moves.StartingEdge with
        | TopEdge x2 -> OnVerticalEdge((x2 - 1) / 2, 0) |> VPoint
        | RightEdge (x2, y2) -> OnHorizontalEdge (x2/2, (y2 - 1)/2) |> HPoint
        | BottomEdge (x2, y2) -> OnVerticalEdge ((x2 - 1) / 2, y2/2) |> VPoint
        | LeftEdge y2 -> OnHorizontalEdge (0, (y2 - 1) / 2) |> HPoint
        
    let (steps, _) =
        moves.Moves
        |> movesToDirections
        |> Seq.fold processDirection ([ ], startingStepPoint)

    let endingStep =
        match moves.EndingEdge with
        | TopEdge x2 -> VStep (OnVerticalEdge ((x2 - 1) / 2, 0), Up)
        | RightEdge (x2, y2) ->
            HStep (OnHorizontalEdge (x2/2, (y2 - 1) / 2), Right)
        | BottomEdge (x2, y2) ->
            VStep (OnVerticalEdge ((x2-1)/2, y2/2), Down)
        | LeftEdge y2 -> HStep (OnHorizontalEdge (0, (y2-1)/2), Left)
    
    let stepsOrdered = (endingStep :: steps) |> List.rev
    ClippedIsoline { Steps = stepsOrdered }    

let movesToSteps (moves: IsolineMoves): Isoline =
    match moves with
    | ClosedIsolineMoves closedMoves -> closedMovesToSteps closedMoves
    | ClippedIsolineMoves clippedMoves ->  clippedMovesToSteps clippedMoves
