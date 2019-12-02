module Demeton.Vectorization.Isolines

type IsolineHPoint = OnHorizontalEdge of (int * int) 
type IsolineVPoint = OnVerticalEdge of (int * int) 

type IsolinePoint =
    | IsolineHPoint of IsolineHPoint 
    | IsolineVPoint of IsolineVPoint

type IsolineHorizontalStepDirection = Left | Right
type IsolineVerticalStepDirection = Up | Down

type IsolineStepDirection =
    | HDirection of IsolineHorizontalStepDirection
    | VDirection of IsolineVerticalStepDirection
    
type IsolineStep =
    | HStep of IsolineHPoint * IsolineHorizontalStepDirection
    | VStep of IsolineVPoint * IsolineVerticalStepDirection

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

type IsolineFunc = IsolinePoint -> IsolineStep option

let oppositeStep = function
    | VStep (point, Up) -> VStep (point, Down)
    | VStep (point, Down) -> VStep (point, Up)
    | HStep (point, Right) -> HStep (point, Left)
    | HStep (point, Left) -> HStep (point, Right)

/// For a given step, returns an array of possible next step directions
/// (not taking into account whether it has reached an edge of the array). 
let allowedDirectionsForStep = function
    | VStep (_, Up) ->
        [| HDirection Left; VDirection Up; HDirection Right |]
    | HStep (_, Right) ->
        [| VDirection Up; HDirection Right; VDirection Down |]
    | VStep (_, Down) ->
        [| HDirection Right; VDirection Down; HDirection Left |]
    | HStep (_, Left) ->
        [| VDirection Down; HDirection Left; VDirection Up |]

/// Builds the next step given the current step and the direction of
/// the next step.
let buildNextStep
    (fromStep: IsolineStep)
    (toStepDirection: IsolineStepDirection)
    : IsolineStep =

    match fromStep, toStepDirection with
    | HStep (fromPoint, fromDirection), HDirection toDirection  ->
        match fromPoint, fromDirection, toDirection with
        | (OnHorizontalEdge (x,y), Left, Left) ->
            HStep (OnHorizontalEdge (x-1, y), toDirection)
        | (OnHorizontalEdge (x,y), Right, Right) ->
            HStep (OnHorizontalEdge (x+1, y), toDirection)
        | _ ->
            sprintf
                "bug: invalid move from point %A and direction %A to direction %A"
                fromPoint fromDirection toStepDirection
            |> invalidOp
    | HStep (fromPoint, fromDirection), VDirection toDirection  ->
        match fromPoint, fromDirection, toDirection with
        | (OnHorizontalEdge (x,y), Left, Up) ->
            VStep (OnVerticalEdge (x-1, y), toDirection)
        | (OnHorizontalEdge (x,y), Left, Down) ->
            VStep (OnVerticalEdge (x-1, y+1), toDirection)
        | (OnHorizontalEdge (x,y), Right, Up) ->
            VStep (OnVerticalEdge (x, y), toDirection)
        | (OnHorizontalEdge (x,y), Right, Down) ->
            VStep (OnVerticalEdge (x, y+1), toDirection)
    | VStep (fromPoint, fromDirection), VDirection toDirection ->
        match fromPoint, fromDirection, toDirection with
        | (OnVerticalEdge (x,y), Up, Up) ->
            VStep (OnVerticalEdge (x, y-1), toDirection)
        | (OnVerticalEdge (x,y), Down, Down) ->
            VStep (OnVerticalEdge (x, y+1), toDirection)
        | _ ->
            sprintf
                "bug: invalid move from point %A and direction %A to direction %A"
                fromPoint fromDirection toStepDirection
            |> invalidOp        
    | VStep (fromPoint, fromDirection), HDirection toDirection ->
        match fromPoint, fromDirection, toDirection with
        | (OnVerticalEdge (x,y), Up, Left) ->
            HStep (OnHorizontalEdge (x, y-1), toDirection)
        | (OnVerticalEdge (x,y), Up, Right) ->
            HStep (OnHorizontalEdge (x+1, y-1), toDirection)
        | (OnVerticalEdge (x,y), Down, Left) ->
            HStep (OnHorizontalEdge (x, y), toDirection)
        | (OnVerticalEdge (x,y), Down, Right) ->
            HStep (OnHorizontalEdge (x+1, y), toDirection)

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
    | HStep (OnHorizontalEdge (x, y), _) ->
        x = 0 || y = 0 || x = (maxHorizX width) || y = (maxHorizY height)  
    | VStep (OnVerticalEdge (x, y), _) -> 
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
    width height isolineFunc step: bool =
    match step with
    | VStep (OnVerticalEdge (x, y), _) ->
        if x >= 0 && x <= (maxVertX width)
           && y >= 0 && y <= (maxVertY height) then
            isolineFunc (IsolineVPoint (OnVerticalEdge (x, y))) |> Option.isSome
        else false
    | HStep (OnHorizontalEdge (x, y), _) ->
        if x >= 0 && x <= (maxHorizX width)
           && y >= 0 && y <= (maxHorizY height) then
            isolineFunc (IsolineHPoint (OnHorizontalEdge (x, y))) |> Option.isSome
        else false

/// For a given array, returns a sequence of identified isolines.
let findIsolines width height (isolineFunc: IsolineFunc)
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
        
    let isEdgeFree = function
        | HStep (OnHorizontalEdge (x, y), _) -> isHorizEdgeFree x y
        | VStep (OnVerticalEdge (x, y), _) -> isVertEdgeFree x y
                    
    /// Lists all the horizontal points detected for an isoline at the specified
    /// array coordinates.
    let listHorizIsolinePoints x y = seq {               
        if isHorizEdgeFree x y then
            let stepDirectionWhenSearching =
                match x, maxHorizX width with
                | (x, max) when x < max -> Right
                | (x, max) when x = max -> Left 
                | _ -> invalidOp "bug"

            let stepPoint = OnHorizontalEdge (x, y)
            let actualStepMaybe = isolineFunc (IsolineHPoint stepPoint)
            match actualStepMaybe with
            | Some (HStep (_, actualStepDirection)) ->
                let searchDirection =
                    if stepDirectionWhenSearching = actualStepDirection then
                        Forward
                    else Backward
                    
                yield (HStep (stepPoint, stepDirectionWhenSearching),
                        searchDirection)
            | Some _ -> invalidOp "bug"
            | None -> ignore()
    }
                    
    /// Lists all the vertical points detected for an isoline at the specified
    /// array coordinates.
    let listVertIsolinePoints x y = seq {
        if isVertEdgeFree x y then
            let stepDirectionWhenSearching =
                match y, maxVertY height with
                | (y, max) when y < max -> Down
                | (y, max) when y = max -> Up
                | _ -> invalidOp "bug"
            
            let stepPoint = OnVerticalEdge (x, y)
            let actualStepMaybe = isolineFunc (IsolineVPoint stepPoint)
            match actualStepMaybe with
            | Some (VStep (_, actualStepDirection)) ->
                let searchDirection =
                    if stepDirectionWhenSearching = actualStepDirection then
                        Forward
                    else Backward
                    
                yield (VStep (stepPoint, stepDirectionWhenSearching),
                        searchDirection)
            | Some _ -> invalidOp "bug"
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
    let findNextStep currentStep (allowedDirections: IsolineStepDirection[])
        : IsolineStep option =
        
        allowedDirections
        |> Array.map (fun possibleDirection ->
            buildNextStep currentStep possibleDirection)
        // filter out directions that don't correspond to isoline paths
        |> Array.filter
               (isOnIsolinePath width height isolineFunc)
        // filter out directions that were already covered by other isolines
        |> Array.filter isEdgeFree
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
    let rec moveIsoline searchDirection firstStep (stepsSoFar: IsolineStep list)
        : IsolineEndingType * IsolineStep list =
        let previousStep = List.head stepsSoFar 
        let allowedDirections = allowedDirectionsForStep previousStep
    
        match findNextStep previousStep allowedDirections with
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
                |> List.map oppositeStep

        match endingType with
        | ClosedEnding -> ClosedIsoline  { Steps = steps }
        | ClippedEnding -> ClippedIsoline  { Steps = steps }
         
    let markIsolinePointAsCovered step =
        match step with
        | HStep (OnHorizontalEdge (x, y), _) ->
            let isEdgeFree = isHorizEdgeFree x y
            if isEdgeFree then
                coverageArrayHorizontal.[array2dIndex x y] <- true
            else
                invalidOp "bug: this point was already covered by an isoline"
        | VStep (OnVerticalEdge (x, y), _) ->
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
