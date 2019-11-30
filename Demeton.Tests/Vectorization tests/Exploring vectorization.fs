module Tests.``Vectorization tests``.``Exploring vectorization``

open Xunit
open Swensen.Unquote

type IsolinePoint =
    | OnHorizontalEdge of (int * int)
    | OnVerticalEdge of (int * int)

type IsolineStepDirection =
    | Up
    | Right
    | Down
    | Left
    
type IsolineStep = (IsolinePoint * IsolineStepDirection) 

type IsolineMovement = IsolineStepDirection list

type Isoline = {
    StartingPoint: IsolinePoint
    Movement: IsolineMovement
}

let findIsoline
    width height (heights: int -> int -> float) isolineValue
    : Isoline option =
        
    /// Determines whether the isoline value is between the left and right
    /// height values.
    let isIsoValueBetween (heightToTheLeft: float) (heightToTheRight: float) =
        match heightToTheLeft, heightToTheRight with
        | (l, r) when l >= r -> false
        | (l, r) when l <= isolineValue && isolineValue <= r -> true
        | _ -> false
        
    /// Tries to find the starting step for a isoline.
    let tryFindIsolineStartingStep(): IsolineStep option =
        let xs = seq { 0 .. width-2 }
        let ys = seq { 0 .. height-2 }
    
        // todo: we must also cover Down and Left cases
        Seq.allPairs xs ys
        |> Seq.tryPick (fun (x, y) ->
            let h00 = heights x y
            let h10 = heights (x + 1) y
            if isIsoValueBetween h00 h10 then
                (OnVerticalEdge (x, y), Up) |> Some
            else
                let h01 = heights x (y + 1)
                if isIsoValueBetween h00 h01 then
                    (OnHorizontalEdge (x, y), Right) |> Some
                else
                    None
        )
    
    /// Calculates the step point given the current step and the direction of
    /// movement.
    let getIsolinePoint
        ((fromPoint, fromDirection): IsolineStep)
        (toStepDirection: IsolineStepDirection)
        : IsolinePoint =
        match fromPoint, fromDirection, toStepDirection with
        | (OnHorizontalEdge (x,y), Left, Up) -> OnVerticalEdge (x-1, y)
        | (OnHorizontalEdge (x,y), Left, Down) -> OnVerticalEdge (x-1, y+1)
        | (OnHorizontalEdge (x,y), Left, Left) -> OnHorizontalEdge (x-1, y)
        | (OnHorizontalEdge (x,y), Right, Up) -> OnVerticalEdge (x, y)
        | (OnHorizontalEdge (x,y), Right, Down) -> OnVerticalEdge (x, y+1)
        | (OnHorizontalEdge (x,y), Right, Right) -> OnHorizontalEdge (x+1, y)
        | (OnVerticalEdge (x,y), Up, Up) -> OnVerticalEdge (x, y-1)
        | (OnVerticalEdge (x,y), Up, Left) -> OnHorizontalEdge (x, y-1)
        | (OnVerticalEdge (x,y), Up, Right) -> OnHorizontalEdge (x+1, y-1)
        | (OnVerticalEdge (x,y), Down, Down) -> OnVerticalEdge (x, y+1)
        | (OnVerticalEdge (x,y), Down, Left) -> OnHorizontalEdge (x, y)
        | (OnVerticalEdge (x,y), Down, Right) -> OnHorizontalEdge (x+1, y)
        | _ ->
            sprintf
                "bug: invalid move from point %A and direction %A to direction %A"
                fromPoint fromDirection toStepDirection
            |> invalidOp
    
    /// Determines whether the given step lies on the isoline path. 
    let isOnIsolinePath step: bool =
        let (hLeft, hRight) =
            match step with
            | (OnVerticalEdge (x, y), Up) -> (heights x y, heights (x+1) y)
            | (OnHorizontalEdge (x, y), Right) -> (heights x y, heights x (y+1))
            | (OnVerticalEdge (x, y), Down) -> (heights (x+1) y, heights x y)
            | (OnHorizontalEdge (x, y), Left) -> (heights x (y+1), heights x y)
            | (point, direction) ->
                sprintf "bug: invalid step - point %A, direction %A" point direction
                |> invalidOp

        hLeft < isolineValue && isolineValue < hRight
    
    /// Finds the next appropriate isoline step based on the specified current
    /// step. If no new steps are possible (when the isoline reaches the array
    /// edge, returns None.
    let findNextStep
        currentStep
        (possibleDirections: IsolineStepDirection[])
        : IsolineStep option =
        
        possibleDirections
        |> Array.map (fun possibleDirection ->
            (getIsolinePoint currentStep possibleDirection,
             possibleDirection))
        |> Array.filter isOnIsolinePath
        |> function
        | [| actualStep |] -> Some actualStep
        | [||] -> invalidOp "todo: no more steps - is this possible?"
        | _ -> invalidOp "bug: more than one possible step"
            
    /// Recursively moves one step of the isoline until it reaches its starting
    /// point, building the visited steps list.
    let rec moveIsoline firstStep stepsSoFar: IsolineStep list =
        let previousStep = List.head stepsSoFar 
        let possibleDirections =
            match previousStep with
            | (_, Up) -> [| Left; Up; Right |]
            | (_, Right) -> [| Up; Right; Down |]
            | (_, Down) -> [| Right; Down; Left |]
            | (_, Left) -> [| Down; Left; Up |]
    
        match findNextStep previousStep possibleDirections with
        | Some nextStep ->
            if nextStep = firstStep then
                stepsSoFar
            else
                moveIsoline firstStep (nextStep :: stepsSoFar)
        | None -> invalidOp "todo"

    /// Traces the whole movement of the isoline, from the specified starting
    /// step.
    let traceIsoline (startingStep: IsolineStep): Isoline =
        let (startingPoint, _) = startingStep
        
        let directions =
            moveIsoline startingStep [ startingStep ]
            |> List.map (fun (_, direction) -> direction)
            |> List.rev
        { StartingPoint = startingPoint; Movement = directions }
    
    tryFindIsolineStartingStep()
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
                Movement = [ Up; Right; Down; Left ]
            } @>
