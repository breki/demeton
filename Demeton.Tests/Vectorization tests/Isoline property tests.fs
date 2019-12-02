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

let ``isolines properties``((heightsArray, isoValueInt): int[,] * int) =
    let heights x y = heightsArray.[x,y] |> float
    
    let width = heightsArray |> Array2D.length1
    let height = heightsArray |> Array2D.length2
    let isoValue = float isoValueInt

    let isolineStepsThatDoNotFollowsPrevious isoline =
        isoline |> isolineSteps
        |> List.pairwise
        |> List.filter (fun (prev, next) -> next |> canFollowStep prev |> not)
        
    let closedIsolineIsProperlyClosed (isoline: Isoline) =
        let steps = isoline |> isolineSteps
        let firstStep = steps.Head 
        let lastStep = steps |> List.last
        
        match isoline with
        | ClosedIsoline _ -> firstStep |> canFollowStep lastStep 
        | ClippedIsoline _ -> true
        
    let clippedIsolineEndsAtEdges (isoline: Isoline) =
        let steps = isoline |> isolineSteps
        let firstStep = steps.Head 
        let lastStep = steps |> List.last
        
        match isoline with
        | ClosedIsoline _ -> true 
        | ClippedIsoline _ ->
            (firstStep |> isStepOnArrayEdge width height)
            && (lastStep |> isStepOnArrayEdge width height)

    let isolineCorrectlyDividesTheSpace isoline =
        isoline |> isolineSteps
        |> List.forall (fun step ->
            isOnIsolinePath
                heights width height isoValue Forward step)    

    let stepIsCovered isolines step =
        if isOnIsolinePath heights width height isoValue Forward step then
            isolines
            |> Array.exists (fun isoline ->
                let stepPoint, stepDirection = step
                let oppositeStep = (stepPoint, oppositeStepDirection stepDirection)
                    
                isoline |> isolineSteps                       
                |> List.exists (fun x -> x = step || x = oppositeStep)
                )
        else true
            
    match width, height with
    | (0, _) -> true |> Prop.classify true "Empty array"
    | (_, 0) -> true |> Prop.classify true "Empty array"
    | _ ->
        let isolines =
            findIsolines width height heights isoValue
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
            |> Seq.forall isolineCorrectlyDividesTheSpace
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
                       
        let allArrayHasBeenCovered() =
            let notCoveredPoints =
                Array.allPairs [|0..width|] [|0..height|]
                |> Array.filter (fun (x, y) ->
                    let horizNotCovered =
                        if x <= (maxHorizX width)
                           && y <= (maxHorizY height) then
                            stepIsCovered
                                isolines (OnHorizontalEdge (x, y), Left) |> not
                        else false
                        
                    let vertNotCovered =
                        if x <= (maxVertX width) && y <= (maxVertY height) then
                            stepIsCovered
                                isolines (OnVerticalEdge (x, y), Up) |> not
                        else false
                        
                    horizNotCovered || vertNotCovered)
                
            notCoveredPoints
            |> Array.isEmpty
            |> Prop.label "all isoline positions in the array have been covered"
            |@ sprintf "isoline positions not covered: %A" notCoveredPoints
            
        allIsolinesHaveCorrectlyConstructedStepsThatFollowPreviousOne
        .&. allIsolinesCorrectlyDivideTheSpace
        .&. allClosedIsolinesAreProperlyClosed
        .&. allClippedIsolinesEndAtEdges
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


