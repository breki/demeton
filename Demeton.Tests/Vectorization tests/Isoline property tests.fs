module Tests.``Vectorization tests``.``Isoline property tests``

open Demeton.Vectorization.Isolines
open Xunit
open FsCheck

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


