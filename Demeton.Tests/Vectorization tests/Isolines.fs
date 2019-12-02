module Demeton.Vectorization.Isolines

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

/// Gets the steps of the isoline.
let isolineSteps = function
    | ClosedIsoline x -> x.Steps
    | ClippedIsoline x -> x.Steps

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

/// For a given step, returns an array of possible next step directions
/// (not taking into account whether it has reached an edge of the array). 
let allowedDirectionsForStep = function
    | (_, Up) -> [| Left; Up; Right |]
    | (_, Right) -> [| Up; Right; Down |]
    | (_, Down) -> [| Right; Down; Left |]
    | (_, Left) -> [| Down; Left; Up |]

/// Builds the next step given the current step and the direction of
/// the next step.
let buildNextStep
    ((fromPoint, fromDirection): IsolineStep)
    (toStepDirection: IsolineStepDirection)
    : IsolineStep =
        
    let nextPoint = 
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
    (nextPoint, toStepDirection)

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
            buildNextStep currentStep possibleDirection)
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
        let allowedDirections = allowedDirectionsForStep previousStep
    
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
        isoline |> isolineSteps
        |> List.iter markIsolinePointAsCovered
    
    findIsolineStartingSteps()
    |> Seq.map (fun x ->
        let isoline = traceIsoline x
        drawIsolineOnCoverageArray isoline      
        isoline)
