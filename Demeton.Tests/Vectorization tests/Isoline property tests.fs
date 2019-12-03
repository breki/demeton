/// Property tests for isoline detection algorithm.
module Tests.``Vectorization tests``.``Isoline property tests``

open Demeton.Vectorization.Isolines
open Xunit
open FsCheck

/// Determines whether two steps are neighboring (i.e. the second one can
/// follow the first one). 
let canFollowStep (fromStep: IsolineStep) (toStep: IsolineStep) =
    fromStep |> allowedDirectionsForStep
    |> Array.map (buildNextStep fromStep)
    |> Array.exists (fun step -> step = toStep)

/// Finds any isoline steps that are not chained together properly.
let isolineStepsThatDoNotFollowsPrevious isoline =
    isoline |> isolineSteps
    |> List.pairwise
    |> List.filter (fun (prev, next) -> next |> canFollowStep prev |> not)

/// Determines whether a closed isoline is closed properly (the first step
/// can be chained with the last one).
let closedIsolineIsProperlyClosed (isoline: Isoline) =
    let steps = isoline |> isolineSteps
    let firstStep = steps.Head 
    let lastStep = steps |> List.last
    
    match isoline with
    | ClosedIsoline _ -> firstStep |> canFollowStep lastStep 
    | ClippedIsoline _ -> true

let ``isolines properties``((heightsArray, isoValueInt): int[,] * int) =
    let width = heightsArray |> Array2D.length1
    let height = heightsArray |> Array2D.length2
    let isoValue = float isoValueInt
    
    /// Segmentation function implemented as a elevation contours identifier.
    let segmentationFunc: SegmentationFunc = fun isolinePoint ->
        match isolinePoint with
        | IsolineHPoint (OnHorizontalEdge (x, y)) ->
            let hUp = heightsArray.[x, y]
            let hDown = heightsArray.[x, y + 1]
            match hUp, isoValue, hDown with
            | _ when hUp <= isoValueInt && isoValueInt < hDown ->
                HStep (OnHorizontalEdge (x, y), Right) |> Some
            | _ when hUp > isoValueInt && isoValueInt >= hDown ->
                HStep (OnHorizontalEdge (x, y), Left) |> Some
            | _ -> None
        | IsolineVPoint (OnVerticalEdge (x, y)) ->
            let hLeft = heightsArray.[x, y]
            let hRight = heightsArray.[x + 1, y]
            match hLeft, isoValue, hRight with
            | _ when hLeft <= isoValueInt && isoValueInt < hRight ->
                VStep (OnVerticalEdge (x, y), Up) |> Some
            | _ when hLeft > isoValueInt && isoValueInt >= hRight ->
                VStep (OnVerticalEdge (x, y), Down) |> Some
            | _ -> None

    /// Determines whether a clipped isoline really ends at the edges of the
    /// array.
    let clippedIsolineEndsAtEdges (isoline: Isoline) =
        let steps = isoline |> isolineSteps
        let firstStep = steps.Head 
        let lastStep = steps |> List.last
        
        match isoline with
        | ClosedIsoline _ -> true 
        | ClippedIsoline _ ->
            let bothEndsAreAtArrayEdges =
                (firstStep |> isStepOnArrayEdge width height)
                && (lastStep |> isStepOnArrayEdge width height)
                
            bothEndsAreAtArrayEdges

    /// Determines whether the isoline is really moving in the way that
    /// separates the space left and right of it in the way the segmentation
    /// function specifies. 
    let isolineCorrectlySegmentsTheSpace isoline =
        isoline |> isolineSteps
        |> List.forall (fun step ->
            isOnIsolinePath width height segmentationFunc step)    

    /// For a given step, find any isolines that cover it. If the step does not
    /// represent an isoline edge, return None. 
    let findIsolinesCoveringStep isolines step =
        if isOnIsolinePath width height segmentationFunc step then
            isolines
            |> Array.filter (fun isoline ->
                let oppositeStep = oppositeStep step
                    
                isoline |> isolineSteps                       
                |> List.exists (fun x -> x = step || x = oppositeStep)
                )
             |> Some
        else None
            
    match width, height with
    | (0, _) -> true |> Prop.classify true "Empty array"
    | (_, 0) -> true |> Prop.classify true "Empty array"
    | _ ->
        let isolines =
            findIsolines width height segmentationFunc
            |> Seq.toArray      
        
        let allIsolinesHaveCorrectlyConstructedStepsThatFollowPreviousOne() =
            let isolinesWithWrongSteps =
                isolines
                |> Seq.map isolineStepsThatDoNotFollowsPrevious
                |> Seq.filter (fun illegalSteps ->
                    illegalSteps |> Seq.isEmpty |> not) 
                
            isolinesWithWrongSteps |> Seq.isEmpty
            |> Prop.label
                   "all isolines have correctly constructed steps that follow previous one"
            |@ sprintf
                   "isolines and their invalid steps: %A" isolinesWithWrongSteps
        
        let allIsolinesCorrectlyDivideTheSpace() =
            isolines
            |> Seq.forall isolineCorrectlySegmentsTheSpace
            |> Prop.label "all isolines correctly divide the space into two areas"
        
        let allClosedIsolinesAreProperlyClosed() =
            let improperlyClosedIsolines =
                isolines
                |> Seq.filter (closedIsolineIsProperlyClosed >> not)
                
            improperlyClosedIsolines |> Seq.isEmpty
            |> Prop.label "all closed isolines are properly closed"
            |@ sprintf
                "improperly closed polyines: %A" improperlyClosedIsolines
        
        let allClippedIsolinesEndAtEdges() =
            isolines
            |> Seq.forall clippedIsolineEndsAtEdges
            |> Prop.label "all clipped isolines end at edges"
                       
        let allArrayPointsHaveBeenCoveredOnceAndExactlyOnce() =
            let incorrectlyCoveredIsolineEdges =
                // for all array positions...
                Array.allPairs [|0..width|] [|0..height|]
                // find any positions that represent isoline edges, but were
                // not covered by one and exactly one isoline
                |> Array.filter (fun (x, y) ->
                    let horizCovered =
                        if x <= (maxHorizX width)
                           && y <= (maxHorizY height) then
                            findIsolinesCoveringStep
                                isolines (HStep (OnHorizontalEdge (x, y), Left))
                            |> Option.map (fun coveringIsolines ->
                                coveringIsolines.Length = 1)
                        else None
                        
                    let vertCovered =
                        if x <= (maxVertX width) && y <= (maxVertY height) then
                            findIsolinesCoveringStep
                                isolines (VStep (OnVerticalEdge (x, y), Up))
                            |> Option.map (fun coveringIsolines ->
                                coveringIsolines.Length = 1)
                        else None
                        
                    // Array edge is incorrectly covered if either horizCovered
                    // or vertCovered is 'Some false'. In that case return true
                    // to the filter.
                    match horizCovered, vertCovered with
                    | (None, None) -> false
                    | (Some true, Some true) -> false
                    | (Some true, None) -> false
                    | (None, Some true) -> false
                    | _ -> true
                )
                
            incorrectlyCoveredIsolineEdges
            |> Array.isEmpty
            |> Prop.label
                   "all isoline edges in the array have been covered once and exactly once"
            |@ sprintf "isoline edges not covered: %A" incorrectlyCoveredIsolineEdges
            
        allIsolinesHaveCorrectlyConstructedStepsThatFollowPreviousOne
        .&. allIsolinesCorrectlyDivideTheSpace
        .&. allClosedIsolinesAreProperlyClosed
        .&. allClippedIsolinesEndAtEdges
        .&. allArrayPointsHaveBeenCoveredOnceAndExactlyOnce

[<Fact>]    
let ``Test isoline properties``() =
    let genHeight = Gen.choose(0, 10)
    let genArray = genHeight |> Gen.array2DOf

    Gen.zip genArray genHeight 
    |> Arb.fromGen
    |> Prop.forAll <| ``isolines properties``
    |> Check.QuickThrowOnFailure
//    |> replayPropertyCheck (1567451850,296676651) // not at the edge


