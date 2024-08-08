/// Contains types and functions for identifying isolines on a rectangular
/// 2D array using the Marching Squares algorithm
/// (https://en.wikipedia.org/wiki/Marching_squares).
module Demeton.Vectorization.MarchingSquares

/// A point on a horizontal edge between two array cells, one above the other.
type HPoint = OnHorizontalEdge of (int * int)
/// A point on a vertical edge between two array cells, one to the left,
/// the other to the right.
type VPoint = OnVerticalEdge of (int * int)

/// A point in marching squares coordinate system, which distinguishes between
/// horizontal and vertical edges.
type Point =
    /// A point on a horizontal edge between two array cells, one above the
    /// other.
    | HPoint of HPoint
    /// A point on a vertical edge between two array cells, one to the left,
    /// the other to the right.
    | VPoint of VPoint

/// A direction of the marching squares step on a horizontal edge.
type HorizontalStepDirection =
    | Left
    | Right

/// A direction of the marching squares step on a vertical edge.
type VerticalStepDirection =
    | Up
    | Down

/// A direction of the marching squares step.
type StepDirection =
    /// A direction of the marching squares step on a horizontal edge.
    | HDirection of HorizontalStepDirection
    /// A direction of the marching squares step on a vertical edge.
    | VDirection of VerticalStepDirection

/// A single step of the isoline, consisting of the point on the marching
/// squares coordinate system and the direction of the step.
type Step =
    // An isoline step on the horizontal edge.
    | HStep of HPoint * HorizontalStepDirection
    // An isoline step on the vertical edge.
    | VStep of VPoint * VerticalStepDirection

/// An isoline that closes on itself (in a loop).
type ClosedIsoline = { Steps: Step list }

/// An isoline that starts and ends on the edges of the array.
type ClippedIsoline = { Steps: Step list }

/// An isoline represented by its marching squares steps.
type Isoline =
    | ClosedIsoline of ClosedIsoline
    | ClippedIsoline of ClippedIsoline

/// A function that partitions the array cells into two distinct values,
/// with isolines representing the boundaries between these two values.
/// For the given point on the isoline coordinate system (the point
/// separating two cells in the array), the function returns the isoline
/// step needed to separate the two cells (if they each belong to a separate
/// segmented value) or None if the two cells belong to the same segmented
/// value.
/// For example, when looking for elevation contours, the segmentation
/// function would separate the array cells into two groups: one for the cells
/// below the contour's elevation and the other for those above this elevation.
type SegmentationFunc = Point -> Step option

/// Describes whether the isoline is closed or clipped one.
type private IsolineEndingType =
    /// An isoline that closes on itself (in a loop).
    | ClippedEnding
    /// An isoline that starts and ends on the edges of the array.
    | ClosedEnding

/// Defines the direction of the isoline search. When Backward, we create
/// isoline with reverse steps and directions, which are then reversed
/// at the end.
type private SearchDirection =
    | Forward
    | Backward

/// Gets the steps of the isoline.
let isolineSteps =
    function
    | ClosedIsoline x -> x.Steps
    | ClippedIsoline x -> x.Steps

/// For a given step, returns the step in the opposite direction.
let oppositeStep =
    function
    | VStep(point, Up) -> VStep(point, Down)
    | VStep(point, Down) -> VStep(point, Up)
    | HStep(point, Right) -> HStep(point, Left)
    | HStep(point, Left) -> HStep(point, Right)

/// For a given step, returns an array of possible next step directions
/// (not taking into account whether it has reached an edge of the array).
let allowedDirectionsForStep =
    function
    | VStep(_, Up) -> [| HDirection Left; VDirection Up; HDirection Right |]
    | HStep(_, Right) -> [| VDirection Up; HDirection Right; VDirection Down |]
    | VStep(_, Down) -> [| HDirection Right; VDirection Down; HDirection Left |]
    | HStep(_, Left) -> [| VDirection Down; HDirection Left; VDirection Up |]

/// Builds the next step given the current step and the direction of
/// the next step.
let buildNextStep (fromStep: Step) (toStepDirection: StepDirection) : Step =

    match fromStep, toStepDirection with
    | HStep(fromPoint, fromDirection), HDirection toDirection ->
        match fromPoint, fromDirection, toDirection with
        | OnHorizontalEdge(x, y), Left, Left ->
            HStep(OnHorizontalEdge(x - 1, y), toDirection)
        | OnHorizontalEdge(x, y), Right, Right ->
            HStep(OnHorizontalEdge(x + 1, y), toDirection)
        | _ ->
            sprintf
                "bug: invalid move from point %A and direction %A to direction %A"
                fromPoint
                fromDirection
                toStepDirection
            |> invalidOp
    | HStep(fromPoint, fromDirection), VDirection toDirection ->
        match fromPoint, fromDirection, toDirection with
        | OnHorizontalEdge(x, y), Left, Up ->
            VStep(OnVerticalEdge(x - 1, y), toDirection)
        | OnHorizontalEdge(x, y), Left, Down ->
            VStep(OnVerticalEdge(x - 1, y + 1), toDirection)
        | OnHorizontalEdge(x, y), Right, Up ->
            VStep(OnVerticalEdge(x, y), toDirection)
        | OnHorizontalEdge(x, y), Right, Down ->
            VStep(OnVerticalEdge(x, y + 1), toDirection)
    | VStep(fromPoint, fromDirection), VDirection toDirection ->
        match fromPoint, fromDirection, toDirection with
        | OnVerticalEdge(x, y), Up, Up ->
            VStep(OnVerticalEdge(x, y - 1), toDirection)
        | OnVerticalEdge(x, y), Down, Down ->
            VStep(OnVerticalEdge(x, y + 1), toDirection)
        | _ ->
            sprintf
                "bug: invalid move from point %A and direction %A to direction %A"
                fromPoint
                fromDirection
                toStepDirection
            |> invalidOp
    | VStep(fromPoint, fromDirection), HDirection toDirection ->
        match fromPoint, fromDirection, toDirection with
        | OnVerticalEdge(x, y), Up, Left ->
            HStep(OnHorizontalEdge(x, y - 1), toDirection)
        | OnVerticalEdge(x, y), Up, Right ->
            HStep(OnHorizontalEdge(x + 1, y - 1), toDirection)
        | OnVerticalEdge(x, y), Down, Left ->
            HStep(OnHorizontalEdge(x, y), toDirection)
        | OnVerticalEdge(x, y), Down, Right ->
            HStep(OnHorizontalEdge(x + 1, y), toDirection)

/// Maximum X coordinate for horizontal marching squares points.
let maxHorizX width = width - 1
/// Maximum Y coordinate for horizontal marching squares points.
let maxHorizY height = height - 2
/// Maximum X coordinate for vertical marching squares points.
let maxVertX width = width - 2
/// Maximum Y coordinate for vertical marching squares points.
let maxVertY height = height - 1

/// Determines whether the specified marching squares point is on the edge
/// of the array.
let isStepOnArrayEdge width height =
    function
    | HStep(OnHorizontalEdge(x, y), _) ->
        x = 0 || y = 0 || x = (maxHorizX width) || y = (maxHorizY height)
    | VStep(OnVerticalEdge(x, y), _) ->
        x = 0 || y = 0 || x = (maxVertX width) || y = (maxVertY height)

/// Determines whether the given step lies on an isoline path.
let isOnIsolinePath width height segmentationFunc step : bool =
    match step with
    | VStep(OnVerticalEdge(x, y), _) ->
        if
            x >= 0 && x <= (maxVertX width) && y >= 0 && y <= (maxVertY height)
        then
            segmentationFunc (VPoint(OnVerticalEdge(x, y))) |> Option.isSome
        else
            false
    | HStep(OnHorizontalEdge(x, y), _) ->
        if
            x >= 0
            && x <= (maxHorizX width)
            && y >= 0
            && y <= (maxHorizY height)
        then
            segmentationFunc (HPoint(OnHorizontalEdge(x, y))) |> Option.isSome
        else
            false

/// Private implementation of findIsolines that does not check for the array size.
let findIsolinesPrivate
    width
    height
    (segmentationFunc: SegmentationFunc)
    : Isoline seq =

    let array2dIndex x y = y * width + x

    /// holds a boolean flag for each horizontal point in the marching squares
    /// coordinate system indicating whether the point was already
    /// covered by an isoline
    let coverageArrayHorizontal = Array.init (width * height) (fun _ -> false)

    /// holds a boolean flag for each vertical point in the marching squares
    /// coordinate system indicating whether the point was already
    /// covered by an isoline
    let coverageArrayVertical = Array.init (width * height) (fun _ -> false)

    /// Indicates whether the horizontal point was already covered by an
    /// isoline.
    let isHorizEdgeFree x y =
        if x < 0 || x > (maxHorizX width) then
            raise (System.ArgumentOutOfRangeException "x")

        if y < 0 || y > (maxHorizY height) then
            raise (System.ArgumentOutOfRangeException "y")

        coverageArrayHorizontal.[array2dIndex x y] |> not

    /// Indicates whether the vertical point was already covered by an
    /// isoline.
    let isVertEdgeFree x y =
        if x < 0 || x > (maxVertX width) then
            raise (System.ArgumentOutOfRangeException "x")

        if y < 0 || y > (maxVertY height) then
            raise (System.ArgumentOutOfRangeException "y")

        coverageArrayVertical.[array2dIndex x y] |> not

    /// Indicates whether the point was already covered by an isoline.
    let isEdgeFree =
        function
        | HStep(OnHorizontalEdge(x, y), _) -> isHorizEdgeFree x y
        | VStep(OnVerticalEdge(x, y), _) -> isVertEdgeFree x y

    /// If the specified horizontal point represents an isoline point, returns
    /// its corresponding step as a single-item sequence.
    let anyHorizIsolineStep x y =
        seq {
            if isHorizEdgeFree x y then
                let stepDirectionWhenSearching =
                    match x, maxHorizX width with
                    | x, max when x < max -> Right
                    | x, max when x = max -> Left
                    | _ -> invalidOp "bug"

                let stepPoint = OnHorizontalEdge(x, y)
                let actualStepMaybe = segmentationFunc (HPoint stepPoint)

                match actualStepMaybe with
                | Some(HStep(_, actualStepDirection)) ->
                    let searchDirection =
                        if stepDirectionWhenSearching = actualStepDirection then
                            Forward
                        else
                            Backward

                    yield
                        (HStep(stepPoint, stepDirectionWhenSearching),
                         searchDirection)
                | Some _ -> invalidOp "bug"
                | None -> ignore ()
        }

    /// If the specified vertical point represents an isoline point, returns
    /// its corresponding step as a single-item sequence.
    let anyVertIsolineStep x y =
        seq {
            if isVertEdgeFree x y then
                let stepDirectionWhenSearching =
                    match y, maxVertY height with
                    | y, max when y < max -> Down
                    | y, max when y = max -> Up
                    | _ -> invalidOp "bug"

                let stepPoint = OnVerticalEdge(x, y)
                let actualStepMaybe = segmentationFunc (VPoint stepPoint)

                match actualStepMaybe with
                | Some(VStep(_, actualStepDirection)) ->
                    let searchDirection =
                        if stepDirectionWhenSearching = actualStepDirection then
                            Forward
                        else
                            Backward

                    yield
                        (VStep(stepPoint, stepDirectionWhenSearching),
                         searchDirection)
                | Some _ -> invalidOp "bug"
                | None -> ignore ()
        }

    /// Returns the next segmentation point (edge) that has not yet been
    /// covered by an isoline.
    let findNextSegmentationEdge () : (Step * SearchDirection) seq =
        seq {
            // First look for possible segmentation points at the edges of the
            // array. This will identify all of the isolines that are clipped
            // at the edges of the array.

            // Start with horizontal edge points first
            for y in 0 .. (maxHorizY height) do
                yield! anyHorizIsolineStep 0 y

                if maxHorizX width > 0 then
                    yield! anyHorizIsolineStep (maxHorizX width) y

            // Now do the vertical edge points
            for x in 0 .. (maxVertX width) do
                yield! anyVertIsolineStep x 0

                if maxVertY height > 0 then
                    yield! anyVertIsolineStep x (maxVertY height)

            // Now look for array's inner points, identifying any remaining
            // isolines (which are not clipped at the edges of the array and
            // are thus closed (self-looping) isolines).
            for y in 0 .. (maxHorizY height) do
                for x in 1 .. ((maxHorizX width) - 1) do
                    yield! anyHorizIsolineStep x y

            for x in 0 .. (maxVertX width) do
                for y in 1 .. ((maxVertY height) - 1) do
                    yield! anyVertIsolineStep x y
        }

    /// Finds the next appropriate isoline step based on the specified current
    /// step. If no new steps are possible (when the isoline reaches the array
    /// edge), returns None.
    let findNextStep
        currentStep
        (allowedDirections: StepDirection[])
        : Step option =

        allowedDirections
        |> Array.map (fun possibleDirection ->
            buildNextStep currentStep possibleDirection)
        // filter out directions that don't correspond to isoline paths
        |> Array.filter (isOnIsolinePath width height segmentationFunc)
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


    /// Recursively moves one step of the isoline at the time until it reaches
    /// its starting point or the edge of the array, building the visited steps
    /// list.
    let rec moveIsoline
        searchDirection
        firstStep
        (stepsSoFar: Step list)
        : IsolineEndingType * Step list =
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
    let traceIsoline (startingStep, searchDirection) : Isoline =
        let endingType, isolineSteps =
            moveIsoline searchDirection startingStep [ startingStep ]

        let steps =
            match searchDirection with
            | Forward -> isolineSteps |> List.rev
            | Backward -> isolineSteps |> List.map oppositeStep

        match endingType with
        | ClosedEnding -> ClosedIsoline { Steps = steps }
        | ClippedEnding -> ClippedIsoline { Steps = steps }

    /// For the given isoline step, marks the corresponding edge as covered.
    let markSegmentationEdgeAsCovered step =
        match step with
        | HStep(OnHorizontalEdge(x, y), _) ->
            let isEdgeFree = isHorizEdgeFree x y

            if isEdgeFree then
                coverageArrayHorizontal.[array2dIndex x y] <- true
            else
                invalidOp "bug: this point was already covered by an isoline"
        | VStep(OnVerticalEdge(x, y), _) ->
            let isEdgeFree = isVertEdgeFree x y

            if isEdgeFree then
                coverageArrayVertical.[array2dIndex x y] <- true
            else
                invalidOp "bug: this point was already covered by an isoline"

    /// Marks all of the edges visited by the specified isoline as covered.
    let markEdgesCoveredByIsoline (isoline: Isoline) =
        isoline |> isolineSteps |> List.iter markSegmentationEdgeAsCovered

    findNextSegmentationEdge ()
    |> Seq.map (fun x ->
        let isoline = traceIsoline x
        markEdgesCoveredByIsoline isoline
        isoline)

/// For a given array and a segmentation function, returns a sequence
/// of identified isolines.
let findIsolines
    width
    height
    (segmentationFunc: SegmentationFunc)
    : Isoline seq =
    match width, height with
    | 0, _ -> Seq.empty
    | _, 0 -> Seq.empty
    | _ -> findIsolinesPrivate width height segmentationFunc
