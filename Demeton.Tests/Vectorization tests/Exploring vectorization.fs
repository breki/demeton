module Tests.``Vectorization tests``.``Exploring vectorization``

open Xunit
open Swensen.Unquote
open FsCheck
open PropertiesHelp

type IsolinePoint =
    | OnHorizontalEdge of (int * int)
    | OnVerticalEdge of (int * int)

type IsolineStepDirection =
    | Up
    | Right
    | Down
    | Left
    
type IsolineStep = (IsolinePoint * IsolineStepDirection) 

type IsolineEndingType = ClippedEnding | ClosedEnding

type ClosedIsoline = {
    Steps: IsolineStep list
}

type ClippedIsoline = {
    Steps: IsolineStep list
}

type Isoline =
    | ClosedIsoline of ClosedIsoline
    | ClippedIsoline of ClippedIsoline

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

/// Maximum X coordinate for horizontal isoline points.
let maxHorizX width = width - 1
/// Maximum Y coordinate for horizontal isoline points.
let maxHorizY height = height - 2
/// Maximum X coordinate for vertical isoline points.
let maxVertX width = width - 2
/// Maximum Y coordinate for vertical isoline points.
let maxVertY height = height - 1

/// Determines whether the specified isoline point is on the edge of the
/// array.
let isStepOnArrayEdge width height = function
    | (OnHorizontalEdge (x, y), _) ->
        x = 0 || y = 0 || x = (maxHorizX width) || y = (maxHorizY height)  
    | (OnVerticalEdge (x, y), _) -> 
        x = 0 || y = 0 || x = (maxVertX width) || y = (maxVertY height)         

/// Determines whether the isoline value is between the left and right
/// height values.
let isIsoValueBetween
    isolineValue (heightToTheLeft: float) (heightToTheRight: float) =
    match heightToTheLeft, heightToTheRight with
    | (l, r) when l >= r -> false
    | (l, r) when l <= isolineValue && isolineValue < r -> true
    | _ -> false

/// Determines whether the given step lies on the isoline path. 
let isOnIsolinePath
    heights width height isolineValue searchDirection step: bool =
    match step with
    | (OnVerticalEdge (x, y), Up) ->
        if x >= 0 && x <= (maxVertX width)
           && y >= 0 && y <= (maxVertY height) then
            Some (heights x y, heights (x+1) y)
        else None
    | (OnHorizontalEdge (x, y), Right) ->
        if x >= 0 && x <= (maxHorizX width)
           && y >= 0 && y <= (maxHorizY height) then
            Some (heights x y, heights x (y+1))
        else None
    | (OnVerticalEdge (x, y), Down) ->
        if x >= 0 && x <= (maxVertX width)
           && y >= 0 && y <= (maxVertY height) then
            Some (heights (x+1) y, heights x y)
        else None                
    | (OnHorizontalEdge (x, y), Left) ->
        if x >= 0 && x <= (maxHorizX width)
           && y >= 0 && y <= (maxHorizY height) then
            Some (heights x (y+1), heights x y)
        else None
    | (point, direction) ->
        sprintf "bug: invalid step - point %A, direction %A" point direction
        |> invalidOp
    |> function
    | Some (hLeft, hRight) ->
        match searchDirection with
        | Forward -> isIsoValueBetween isolineValue hLeft hRight
        | Backward -> isIsoValueBetween isolineValue hRight hLeft
    | None -> false

/// For a given array, returns a sequence of identified isolines.
let findIsolines
    width height (heights: int -> int -> float) isolineValue
    : Isoline seq =
        
    let array2dIndex x y = y * width + x
        
    /// indicates whether each horizontal point was already covered by an isoline
    let coverageArrayHorizontal = Array.init (width * height) (fun _ -> false)
    /// indicates whether each vertical point was already covered by an isoline
    let coverageArrayVertical = Array.init (width * height) (fun _ -> false)      
                      
    let isHorizEdgeFree x y =
        if x < 0 || x > (maxHorizX width) then
            raise (System.ArgumentOutOfRangeException "x")
        if y < 0 || y > (maxHorizY height) then
            raise (System.ArgumentOutOfRangeException "y")
        
        coverageArrayHorizontal.[array2dIndex x y] |> not

    let isVertEdgeFree x y =
        if x < 0 || x > (maxVertX width) then
            raise (System.ArgumentOutOfRangeException "x")
        if y < 0 || y > (maxVertY height) then
            raise (System.ArgumentOutOfRangeException "y")
                
        coverageArrayVertical.[array2dIndex x y] |> not
        
    let isPointFree = function
        | OnHorizontalEdge (x, y) -> isHorizEdgeFree x y
        | OnVerticalEdge (x, y) -> isVertEdgeFree x y
                    
    /// Lists all the horizontal points detected for an isoline at the specified
    /// array coordinates.
    let listHorizIsolinePoints x y = seq {
        let h00 = heights x y
                
        if y <= (maxHorizY height) then
            let stepDirectionWhenSearchingMaybe =
                match x, maxHorizX width with
                | (x, max) when x < max -> Some Right
                | (x, max) when x = max -> Some Left
                | _ -> None

            match stepDirectionWhenSearchingMaybe with
            | Some stepDirectionWhenSearching -> 
                if isHorizEdgeFree x y then
                    let h01 = heights x (y + 1)

                    let actualStepDirectionMaybe =
                        if isIsoValueBetween isolineValue h00 h01 then
                            Some Right
                        else if isIsoValueBetween isolineValue h01 h00 then
                            Some Left
                        else None

                    match actualStepDirectionMaybe with
                    | Some actualStepDirection ->
                        let searchDirection =
                            if stepDirectionWhenSearching = actualStepDirection then
                                Forward
                            else Backward
                            
                        yield
                            ((OnHorizontalEdge (x, y), stepDirectionWhenSearching),
                            searchDirection)
                    | None -> ignore()
            | None -> ignore()
    }
                    
    /// Lists all the vertical points detected for an isoline at the specified
    /// array coordinates.
    let listVertIsolinePoints x y = seq {
        let h00 = heights x y
        
        // todo eliminate this case by not calling it with x > maxVertX width
        if x <= (maxVertX width) then
            let stepDirectionWhenSearchingMaybe =
                match y, maxVertY height with
                | (y, max) when y < max -> Some Down
                | (y, max) when y = max -> Some Up
                | _ -> None
            
            match stepDirectionWhenSearchingMaybe with
            | Some stepDirectionWhenSearching -> 
                if isVertEdgeFree x y then
                    let h10 = heights (x + 1) y
                    
                    let actualStepDirectionMaybe =
                        if isIsoValueBetween isolineValue h00 h10 then
                            Some Up
                        else if isIsoValueBetween isolineValue h10 h00 then
                            Some Down
                        else None
                        
                    match actualStepDirectionMaybe with
                    | Some actualStepDirection ->
                        let searchDirection =
                            if stepDirectionWhenSearching = actualStepDirection then
                                Forward
                            else Backward
                            
                        yield
                            ((OnVerticalEdge (x, y), stepDirectionWhenSearching),
                            searchDirection)
                    | None -> ignore()
            | None -> ignore()
    }
                          
    /// Returns a sequence of starting steps of isolines.
    let findIsolineStartingSteps(): (IsolineStep * SearchDirection) seq =
        seq {
            // First look for possible isoline points at the edges of the array.
            // This will identify all of the isolines that are clipped at the
            // edges of the array.
            
            // Start with horizontal edge points first
            for y in 0 .. (maxHorizY height) do
                yield! listHorizIsolinePoints 0 y
                
                if maxHorizX width > 0 then
                    yield! listHorizIsolinePoints (maxHorizX width) y
            
            // Now do the vertical edge points
            for x in 0 .. (maxVertX width) do
                yield! listVertIsolinePoints x 0
                
                if maxVertY height > 0 then
                    yield! listVertIsolinePoints x (maxVertY height)
                            
            // Now look for array's inner points, identifying any remaining
            // isolines (which are not clipped at the edges of the array and
            // are thus closed (self-looping) isolines).
            for y in 0 .. (maxHorizY height) do
                for x in 1 .. ((maxHorizX width) - 1) do
                    yield! listHorizIsolinePoints x y
            for x in 0 .. (maxVertX width) do
                for y in 1 .. ((maxVertY height) - 1) do
                    yield! listVertIsolinePoints x y
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
    
    
    /// Finds the next appropriate isoline step based on the specified current
    /// step. If no new steps are possible (when the isoline reaches the array
    /// edge, returns None.
    let findNextStep
        currentStep
        searchDirection
        (allowedDirections: IsolineStepDirection[])
        : IsolineStep option =
        
        allowedDirections
        |> Array.map (fun possibleDirection ->
            (getIsolinePoint currentStep possibleDirection,
             possibleDirection))
        // filter out directions that don't correspond to isoline paths
        |> Array.filter
               (isOnIsolinePath
                    heights width height isolineValue searchDirection)
        // filter out directions that were already covered by other isolines
        |> Array.filter (fun (point, _) -> isPointFree point)
        |> function
        | [| possibleStep |] -> Some possibleStep
        | [||] ->
            // If the isoline has no more possible steps, this could only mean
            // it has reached an edge of the array. Verify this.
            if not (isStepOnArrayEdge width height currentStep) then
                sprintf
                    "bug: isoline has run out of possible steps, but it has not reached an edge of the heights array (%A)"
                    currentStep
                |> invalidOp
            None
        | possibleSteps ->
            // if there are multiple possible steps, chose the first one
            possibleSteps |> Array.head |> Some
            
    /// Recursively moves one step of the isoline until it reaches its starting
    /// point, building the visited steps list.
    let rec moveIsoline searchDirection firstStep stepsSoFar
        : IsolineEndingType * IsolineStep list =
        let previousStep = List.head stepsSoFar 
        let allowedDirections =
            match previousStep with
            | (_, Up) -> [| Left; Up; Right |]
            | (_, Right) -> [| Up; Right; Down |]
            | (_, Down) -> [| Right; Down; Left |]
            | (_, Left) -> [| Down; Left; Up |]
    
        match findNextStep previousStep searchDirection allowedDirections with
        | Some nextStep ->
            // did we reach the starting point, making a loop?
            if nextStep = firstStep then
                (ClosedEnding, stepsSoFar)
            else
                moveIsoline searchDirection firstStep (nextStep :: stepsSoFar)
        // did we reach an edge of the array?
        | None -> (ClippedEnding, stepsSoFar)

    /// Traces the whole movement of the isoline, from the specified starting
    /// step.
    let traceIsoline (startingStep, searchDirection): Isoline =
        let (endingType, isolineSteps) =
            moveIsoline searchDirection startingStep [ startingStep ]
        
        let steps =
            match searchDirection with
            | Forward -> isolineSteps |> List.rev               
            | Backward ->
                isolineSteps
                |> List.map (fun (point, stepDirection) ->
                    (point, oppositeStepDirection stepDirection))

        match endingType with
        | ClosedEnding -> ClosedIsoline  { Steps = steps }
        | ClippedEnding -> ClippedIsoline  { Steps = steps }
         
    let markIsolinePointAsCovered step =
        match step with
        | (OnHorizontalEdge (x, y), _) ->
            let isEdgeFree = isHorizEdgeFree x y
            if isEdgeFree then
                coverageArrayHorizontal.[array2dIndex x y] <- true
            else
                invalidOp "bug: this point was already covered by an isoline"
        | (OnVerticalEdge (x, y), _) ->
            let isEdgeFree = isVertEdgeFree x y
            if isEdgeFree then
                coverageArrayVertical.[array2dIndex x y] <- true
            else
                invalidOp "bug: this point was already covered by an isoline"
    
    let drawIsolineOnCoverageArray (isoline: Isoline) =
        match isoline with
        | ClosedIsoline x -> x.Steps
        | ClippedIsoline x -> x.Steps
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
  
let ``isolines properties``((heightsArray, isoValueInt): int[,] * int) =
    let heights x y = heightsArray.[x,y] |> float
    
    let width = heightsArray |> Array2D.length1
    let height = heightsArray |> Array2D.length2
    let isoValue = float isoValueInt
    
    match width, height with
    | (0, _) -> true |> Prop.classify true "Empty array"
    | (_, 0) -> true |> Prop.classify true "Empty array"
    | _ ->
        let isolines =
            findIsolines width height heights isoValue
            |> Seq.toArray

        let isolineStepsAreValid isoline =
            match isoline with
            | ClosedIsoline isoline -> isoline.Steps
            | ClippedIsoline isoline -> isoline.Steps
            |> List.forall (fun step ->
                isOnIsolinePath
                    heights width height isoValue Forward step)
        
        let isolineIsProperlyCompleted (isoline: Isoline) =
            match isoline with
            | ClosedIsoline _ -> true
            | ClippedIsoline isoline ->
                (isoline.Steps.Head |> isStepOnArrayEdge width height)
                && (isoline.Steps |> List.last |> isStepOnArrayEdge width height)
        
        let allIsolineStepsAreValid() =
            isolines
            |> Seq.forall isolineStepsAreValid
            |> Prop.label "all isolines are valid"
        
        let allIsolinesAreProperlyCompleted() =
            isolines
            |> Seq.forall isolineIsProperlyCompleted
            |> Prop.label "all isolines are properly completed"
            
        let stepIsCovered step =
            if isOnIsolinePath heights width height isoValue Forward step then
                isolines
                |> Array.exists (fun isoline ->
                    let steps =
                        match isoline with
                        | ClosedIsoline x -> x.Steps
                        | ClippedIsoline x -> x.Steps
                        
                    let stepPoint, stepDirection = step
                    let oppositeStep = (stepPoint, oppositeStepDirection stepDirection)
                        
                    steps
                    |> List.exists (fun x -> x = step || x = oppositeStep)
                    )
            else true
            
        let allArrayHasBeenCovered() =
            let notCoveredPoints =
                Array.allPairs [|0..width|] [|0..height|]
                |> Array.filter (fun (x, y) ->
                    let horizNotCovered =
                        if x <= (maxHorizX width)
                           && y <= (maxHorizY height) then
                            stepIsCovered (OnHorizontalEdge (x, y), Left) |> not
                        else false
                        
                    let vertNotCovered =
                        if x <= (maxVertX width) && y <= (maxVertY height) then
                            stepIsCovered (OnVerticalEdge (x, y), Up) |> not
                        else false
                        
                    horizNotCovered || vertNotCovered)
                
            notCoveredPoints
            |> Array.isEmpty
            |> Prop.label "all isoline positions in the array have been covered"
            |@ sprintf "isoline positions not covered: %A" notCoveredPoints
            
        allIsolineStepsAreValid .&. allIsolinesAreProperlyCompleted
            .&. allArrayHasBeenCovered

[<Fact>]    
let ``Test isoline properties``() =
    let genHeight = Gen.choose(0, 10)
    let genArray = genHeight |> Gen.array2DOf

    Gen.zip genArray genHeight 
    |> Arb.fromGen
    |> Prop.forAll <| ``isolines properties``
    |> Check.QuickThrowOnFailure
//    |> replayPropertyCheck (1567451850,296676651) // not at the edge
     