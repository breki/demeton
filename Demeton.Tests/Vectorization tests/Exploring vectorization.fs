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

type Isoline = {
    Steps: IsolineStep list
}

/// Defines the direction of the isoline search. When Backward, we create
/// isoline with reverse steps and directions, which are then reversed
/// at the end.
type SearchDirection = Forward | Backward

let oppositeStepDirection =
    function
    | Up -> Down
    | Right -> Left
    | Down -> Up
    | Left -> Right

let findIsolines
    width height (heights: int -> int -> float) isolineValue
    : Isoline seq =
        
    let array2dIndex x y = y * height + x
    
    /// indicates whether each horizontal point was already covered by an isoline
    let coverageArrayHorizontal = Array.init (width * height) (fun _ -> false)
    /// indicates whether each vertical point was already covered by an isoline
    let coverageArrayVertical = Array.init (width * height) (fun _ -> false)      
        
    /// Determines whether the isoline value is between the left and right
    /// height values.
    let isIsoValueBetween (heightToTheLeft: float) (heightToTheRight: float) =
        match heightToTheLeft, heightToTheRight with
        | (l, r) when l >= r -> false
        | (l, r) when l <= isolineValue && isolineValue <= r -> true
        | _ -> false
               
    /// Returns a sequence of starting steps of isolines.
    let findIsolineStartingSteps(): (IsolineStep * SearchDirection) seq =
        seq {
            for y in 0 .. height-2 do
                for x in 0 .. width-2 do
                    let horizontalLineFree =
                        coverageArrayHorizontal.[array2dIndex x y] |> not
                    let verticalLineFree =
                        coverageArrayVertical.[array2dIndex x y] |> not
                    
                    let h00 = heights x y
                    let h10 = heights (x + 1) y
                    let h01 = heights x (y + 1)
                    if verticalLineFree && isIsoValueBetween h00 h10 then
                        yield ((OnVerticalEdge (x, y), Down), Backward)
                    if verticalLineFree && isIsoValueBetween h10 h00 then
                        yield ((OnVerticalEdge (x, y), Down), Forward)
                    if horizontalLineFree && isIsoValueBetween h00 h01 then
                        yield ((OnHorizontalEdge (x, y), Right), Forward)
                    if horizontalLineFree && isIsoValueBetween h01 h00 then
                        yield ((OnHorizontalEdge (x, y), Right), Backward)
        }
    
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
    let isOnIsolinePath searchDirection step: bool =
        match step with
        | (OnVerticalEdge (x, y), Up) ->
            if x >= 0 && x + 1 < width && y >= 0 && y < height then
                Some (heights x y, heights (x+1) y)
            else None
        | (OnHorizontalEdge (x, y), Right) ->
            if x >= 0 && x < width && y >= 0 && y + 1 < height then
                Some (heights x y, heights x (y+1))
            else None
        | (OnVerticalEdge (x, y), Down) ->
            if x >= 0 && x + 1 < width && y >= 0 && y < height then
                Some (heights (x+1) y, heights x y)
            else None                
        | (OnHorizontalEdge (x, y), Left) ->
            if x >= 0 && x < width && y >= 0 && y + 1 < height then
                Some (heights x (y+1), heights x y)
            else None
        | (point, direction) ->
            sprintf "bug: invalid step - point %A, direction %A" point direction
            |> invalidOp
        |> function
        | Some (hLeft, hRight) ->
            match searchDirection with
            | Forward -> isIsoValueBetween hLeft hRight
            | Backward -> isIsoValueBetween hRight hLeft
        | None -> false
    
    /// Finds the next appropriate isoline step based on the specified current
    /// step. If no new steps are possible (when the isoline reaches the array
    /// edge, returns None.
    let findNextStep
        currentStep
        searchDirection
        (possibleDirections: IsolineStepDirection[])
        : IsolineStep option =
        
        possibleDirections
        |> Array.map (fun possibleDirection ->
            (getIsolinePoint currentStep possibleDirection,
             possibleDirection))
        |> Array.filter (isOnIsolinePath searchDirection)
        |> function
        | [| actualStep |] -> Some actualStep
        | [||] -> None
        | _ -> invalidOp "bug: more than one possible step"
            
    /// Recursively moves one step of the isoline until it reaches its starting
    /// point, building the visited steps list.
    let rec moveIsoline searchDirection firstStep stepsSoFar: IsolineStep list =
        let previousStep = List.head stepsSoFar 
        let possibleDirections =
            match previousStep with
            | (_, Up) -> [| Left; Up; Right |]
            | (_, Right) -> [| Up; Right; Down |]
            | (_, Down) -> [| Right; Down; Left |]
            | (_, Left) -> [| Down; Left; Up |]
    
        match findNextStep previousStep searchDirection possibleDirections with
        | Some nextStep ->
            // did we reach the starting point, making a loop?
            if nextStep = firstStep then
                stepsSoFar
            else
                moveIsoline searchDirection firstStep (nextStep :: stepsSoFar)
        // did we reach an edge of the array?
        | None -> stepsSoFar

    /// Traces the whole movement of the isoline, from the specified starting
    /// step.
    let traceIsoline (startingStep, searchDirection): Isoline =
        let isolineSteps =
            moveIsoline searchDirection startingStep [ startingStep ]
        
        let steps =
            match searchDirection with
            | Forward -> isolineSteps |> List.rev               
            | Backward ->
                isolineSteps
                |> List.map (fun (point, stepDirection) ->
                    (point, oppositeStepDirection stepDirection))

        { Steps = steps }
         
    let markIsolinePointAsCovered step =
        match step with
        | (OnHorizontalEdge (x, y), _) ->
            let alreadyCovered = coverageArrayHorizontal.[array2dIndex x y]
            if alreadyCovered then
                invalidOp "bug: this point was already covered by an isoline"
            else
                coverageArrayHorizontal.[array2dIndex x y] <- true
        | (OnVerticalEdge (x, y), _) ->
            let alreadyCovered = coverageArrayVertical.[array2dIndex x y]
            if alreadyCovered then
                invalidOp "bug: this point was already covered by an isoline"
            else
                coverageArrayVertical.[array2dIndex x y] <- true
    
    let drawIsolineOnCoverageArray isoline =
        isoline.Steps
        |> List.iter markIsolinePointAsCovered
    
    findIsolineStartingSteps()
    |> Seq.map (fun x ->
        let isoline = traceIsoline x
        drawIsolineOnCoverageArray isoline      
        isoline)

let getHeight (array: float[]) width height x y =
    match x, y with
    | _ when x < 0 -> raise (System.ArgumentOutOfRangeException "x") 
    | _ when x >= width -> raise (System.ArgumentOutOfRangeException "x") 
    | _ when y < 0 -> raise (System.ArgumentOutOfRangeException "y")
    | _ when y >= height -> raise (System.ArgumentOutOfRangeException "y")
    | _ -> array.[y * height + x]

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
    
    test <@ isolines = [ {
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
    
    test <@ isolines = [ {
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
    
    test <@ isolines = [ {
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
    
    test <@ isolines = [ {
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
    
    test <@ isolines = [ {
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
    
    test <@ isolines = [ {
                Steps = [
                    OnVerticalEdge (0, 0), Down
                    OnVerticalEdge (0, 1), Down
                    OnVerticalEdge (0, 2), Down
                ]
            } ] @>
