module Tests.``Data structures``.``Interval trees``.``Interval tree tests``

open Tests.``Data structures``.``Interval trees``.``Interval tree testbed``

open DataStructures
open DataStructures.ListEx

open DataStructures.BinaryTrees
open Xunit
open FsCheck
open PropertiesHelp
open TestLog

// https://www.geeksforgeeks.org/interval-tree/

let logger = initLog "c:\\temp\\logs\\interval-tree.log"
let log message = logger.Information message     

let private ``interval tree properties`` operations =
    let mutable nextItemTag = 'A'
        
    let newInterval (low, high) =
        let item = { Low = low; High = high; Tag = nextItemTag |> string }
        nextItemTag <- ((nextItemTag |> int) + 1) |> char
        item

    /// Executes a test operation on the tree.
    let processOperation
        (state: TreeTestCurrent) operation: TreeTestResult =
        let newState list tree =
            {
                List = list; Tree = tree
                OperationsPerformed = state.OperationsPerformed + 1
            } |> Ok 

        let list = state.List
        let tree = state.Tree
                
        match operation with
        | Insert intervalLowHigh ->
            let interval = newInterval intervalLowHigh
            
            let list' = list |> insertIntoList interval
            let tree' = tree |> IntervalTree.insert lowValue highValue interval
            
            newState list' tree'
        | Remove vector ->
            let itemIndex =
                ((list.Length |> float) - 1.) * vector
                |> System.Math.Round |> int
            if itemIndex >= 0 && itemIndex < (list.Length - 1) then
                let itemToRemove = list.[itemIndex]
                let list' = list |> removeAt itemIndex
                let tree' =
                    tree |> IntervalTree.remove lowValue highValue itemToRemove
                newState list' tree'
            else
                newState list tree
        | Overlapping (low, high) ->
            let overlappingInTestOracle =
                list
                |> findOverlapping low high
                |> orderIntervals
            let overlappingInTree =
                tree
                |> (IntervalTree.findOverlapping lowValue highValue) low high
                |> Seq.toList
                |> orderIntervals
            if overlappingInTree = overlappingInTestOracle then
                newState list tree
            else
                (state,
                    sprintf
                        "Overlapping (%d-%d) result differs from test oracle. Tree result: %A, oracle: %A"
                        low high overlappingInTree overlappingInTestOracle)
                |> Error
                
    let foldFunc
        (verifyProperties: TreePropertyVerificationFunc list) 
        (resultState: TreeTestResult)
        operation: TreeTestResult =
            
        let operationResult =
            match resultState with
            | Ok state -> processOperation state operation
            | Error error -> Error error
            
        let result =
            verifyProperties
            |> List.fold (fun result property ->
                match result with
                | Ok state -> property state
                | Error error -> Error error)
                operationResult

        // uncomment to enable logging of the tree on each step                
//        match result with
//        | Ok state -> log (state.Tree |> treeToAscii)
//        | Error (state, _) -> log (state.Tree |> treeToAscii)
//        log "------------------------"
        
        result

    let classifyByTreeSize size property =
        property
        |> Prop.classify (size <= 5) "final tree size <= 5 elements"
        |> Prop.classify
               (size > 5 && size <= 10) "final tree size 6-10 elements"
        |> Prop.classify
               (size > 10 && size <= 20) "final tree size 11-20 elements"
        |> Prop.classify (size > 20) "final tree size >= 20 elements"
    
    let initialState =
        Ok { List = []; Tree = AvlTree.None; OperationsPerformed = 0 }
        
    let properties = []
        
    let finalState =
        operations
        |> Seq.fold (foldFunc properties) initialState

    match finalState with
    | Result.Ok finalStateOk ->
        let finalTreeElements =
            finalStateOk.Tree
            |> (IntervalTree.intervals lowValue highValue) |> Seq.toList   
        (finalTreeElements = finalStateOk.List)
        |> classifyByTreeSize (finalStateOk.List |> List.length)
        |> Prop.label "the final tree does not have expected intervals"
        |@ sprintf "%A <> %A" finalTreeElements finalStateOk.List
    | Result.Error (errorState, message) -> 
        let errorTreeElements =
            errorState.Tree
            |> (IntervalTree.intervals lowValue highValue) |> Seq.toList   
        false
        |> Prop.label message
        |@ sprintf
               "after %d operations, tree=%A, oracle=%A"
               errorState.OperationsPerformed
               errorTreeElements
               errorState.List

type IntervalTreePropertyTest
    (output: Xunit.Abstractions.ITestOutputHelper) =

    [<Fact>]
    member this.``Interval tree properties``() =
        let genInterval =
            Gen.zip (Gen.choose(0, 1000)) (Gen.choose(0, 1000))
            |> Gen.map (fun (a, b) -> (min a b, max a b))
        
        let genInsert = genInterval |> Gen.map Insert
        let genRemove = (floatFrom0To1Inclusive 1000) |> Gen.map Remove
        let genOverlapping = genInterval |> Gen.map Overlapping
        
        let genOperation = Gen.frequency [
            (20, genInsert); (1, genRemove); (3, genOverlapping)
        ]
        
        let gen = Gen.arrayOf genOperation
        
        ``interval tree properties``
        |> checkPropertyWithTestSize gen output 200 100
//        |> checkPropertyVerboseWithTestSize gen output 200 100
//        |> replayPropertyCheck gen output (9157400,296683675)
