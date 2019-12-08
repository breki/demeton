module Tests.``Vectorization tests``.``Steps to moves transformation``

open Demeton.Vectorization.MarchingSquares
open Demeton.Vectorization.IsolineMoves
open Tests.``Vectorization tests``.SampleSegmentation
open Tests.``Vectorization tests``.``Isoline DSL``
open Tests.``Vectorization tests``.``Marching squares property tests``

open Xunit
open Swensen.Unquote
open FsCheck
open PropertiesHelp

[<Fact>]
let ``Simple clipped isoline left``() =
    let isoline = parseIsolineDef "l;h0,0;l"
    
    test <@ stepsToMoves isoline = ClippedIsolineMoves {
        StartingEdge = RightEdge (0, 1)
        EndingEdge = LeftEdge 1
        Moves = [ ]
    } @>
    test <@ isoline |> stepsToMoves |> movesToSteps = isoline @>

[<Fact>]    
let ``Simple clipped isoline right``() =
    let isoline = parseIsolineDef "l;h0,0;r"
    
    test <@ stepsToMoves isoline = ClippedIsolineMoves {
        StartingEdge = LeftEdge 1
        EndingEdge = RightEdge (0, 1)
        Moves = [ ]
    } @>
    test <@ isoline |> stepsToMoves |> movesToSteps = isoline @>

[<Fact>]    
let ``Two steps clipped isoline``() =
    let isoline = parseIsolineDef "l;h1,0;ld"
    
    test <@ stepsToMoves isoline = ClippedIsolineMoves
                { StartingEdge = RightEdge (2, 1)
                  EndingEdge = BottomEdge (1, 2)
                  Moves = [ moveSW 1 ] } @>
    test <@ isoline |> stepsToMoves |> movesToSteps = isoline @>

[<Fact>]
let ``Simplest closed isoline``() =
    let isoline = parseIsolineDef "o;v1,2;drul"

    test <@ stepsToMoves isoline = ClosedIsolineMoves
                { StartingPoint2 = (3, 4);
                  Moves = [ moveSE 1; moveNE 1; moveNW 1; moveSW 1 ] } @>
    test <@ isoline |> stepsToMoves |> movesToSteps = isoline @>
    

type IsolineMovesPropertyTests(output: Xunit.Abstractions.ITestOutputHelper) =
    let isolineMoves = function
        | ClosedIsolineMoves isoline -> isoline.Moves
        | ClippedIsolineMoves isoline -> isoline.Moves

    let isolineMovesCount = function
        | ClosedIsolineMoves isoline ->
            isoline.Moves
            |> Seq.sumBy (fun move -> move.Count)
        | ClippedIsolineMoves isoline ->
            isoline.Moves
            |> Seq.sumBy (fun move -> move.Count)
            |> (+) 1
    
    let findIsolinesThatDoNotTransformToMovesAndBackToSameSteps isolines =
        isolines
        |> Array.filter (fun (isoline, _, isolineBack) ->
            isoline <> isolineBack)
    
    let findIsolinesThatHaveNonMergedMoves isolines =
        isolines
        |> Seq.filter (fun (_, moves, _) ->
            moves
            |> isolineMoves
            |> Seq.pairwise
            |> Seq.exists (fun (movePrev, moveNext) ->
                    movePrev.Direction = moveNext.Direction))
    
    let findIsolinesThatHaveInconsistentMovesCount isolines =
        isolines
        |> Seq.filter (fun (isoline, moves, _) ->
            (isolineSteps isoline).Length <> (isolineMovesCount moves))
    
    let isolineProperty findOffendingIsolinesFunc label isolines =
        let offendingIsolines = findOffendingIsolinesFunc isolines
        
        offendingIsolines |> Seq.isEmpty
        |> Prop.label label
        |@ sprintf "Offending isolines: %A" offendingIsolines
    
    let propTransformationToMovesIsReversible =
        isolineProperty
            findIsolinesThatDoNotTransformToMovesAndBackToSameSteps
           "transformation from steps to moves and back produces the same steps"
    
    let propMovesOfSameDirectionAreMerged =
        isolineProperty
            findIsolinesThatHaveNonMergedMoves
            "all consecutive isoline moves of the same direction must be merged"
    
    let propMovesCountCorrespondsToStepsCount =
        isolineProperty
            findIsolinesThatHaveInconsistentMovesCount
            "isoline moves count must correspond to the steps count"
       
    let ``isoline moves properties``((heightsArray, isolineHeight): int[,] * int) =
        let width = heightsArray |> Array2D.length1
        let height = heightsArray |> Array2D.length2

        let isolines =
            findIsolines
                width height (heightsSegmentation heightsArray isolineHeight)
            |> Seq.toArray
            |> Array.map (fun isoline ->
                let moves = stepsToMoves isoline
                let isolineBack = movesToSteps moves
                (isoline, moves, isolineBack))

        (propTransformationToMovesIsReversible isolines)                           
        .&. (propMovesOfSameDirectionAreMerged isolines)                           
        .&. (propMovesCountCorrespondsToStepsCount isolines)                           
        |> classifyTestArray width height
                
    [<Fact>]    
    member this.``Test isoline moves properties``() =
        let genHeight = Gen.choose(0, 10)
        let genArray = genHeight |> Gen.array2DOf
        let gen = Gen.zip genArray genHeight

        ``isoline moves properties``
        |> checkPropertyWithTestSize gen output 200 250
