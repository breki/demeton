module Tests.``Data structures``.``Binary search tree tests``

open Demeton.DataStructures.BinarySearchTree

open Xunit
open FsCheck
open PropertiesHelp
open Text
open Text
open Text

let private removeAt itemIndex list =
    let (left, right) = list |> List.splitAt itemIndex
    List.append left (right |> List.tail)

let private nodeToDot (node: BinarySearchTree) (nodeCounter: int) output =
    match node with
    | Some node ->
        let nodeId = sprintf "%d" nodeCounter
        output
        |> appendFormat "{0} [label={1}]" [| nodeId; node.Value |]
        |> newLine |> ignore        
        nodeId
    | None ->
        let nodeId = sprintf "null%d" nodeCounter
        output
        |> appendFormat "{0} [shape=point]" [| nodeId |]
        |> newLine |> ignore
        nodeId

let rec private subtreeToDot (node: BinarySearchTree) (nodeCounter: int) output: int =
    let nodeId = nodeToDot node nodeCounter output
    
    match node with
    | None -> nodeCounter
    | Some node ->
        let nodeCounterBeforeLeft = nodeCounter + 1
        
        let leftNodeId = nodeToDot (node.Left) nodeCounterBeforeLeft output

        output
        |> appendFormat "{0} -> {1}" [| nodeId; leftNodeId |]
        |> newLine |> ignore

        let nodeCounterBeforeRight =
            match node.Left with
            | None -> nodeCounterBeforeLeft + 1
            | Some _ ->
                output
                |> subtreeToDot node.Left (nodeCounterBeforeLeft + 1)

        let rightNodeId = nodeToDot (node.Right) nodeCounterBeforeRight output
        output
        |> appendFormat "{0} -> {1}" [| nodeId; rightNodeId |]
        |> newLine |> ignore

        let nodeCounterAfterRight =
            match node.Right with
            | None -> nodeCounterBeforeRight + 1
            | Some _ ->
                output
                |> subtreeToDot node.Right (nodeCounterBeforeRight + 1)

        nodeCounterAfterRight

let treeToDot (tree: BinarySearchTree) =
    match tree with
    | None -> ""
    | Some _ ->
        let builder =
            buildString()
            |> appendLine "digraph BST {"
        
        builder |> subtreeToDot tree 0 |> ignore
        
        builder
        |> appendLine "}"
        |> toString

type private TreeOperation =
    | Insert of int
    | Remove of float
    | TryRemove of int

type private TreeTestCurrent = {
    List: int list
    Tree: BinarySearchTree
    OperationsPerformed: int
}

type private TreeTestState = Result<TreeTestCurrent, TreeTestCurrent>

let private ``binary search tree properties`` operations =
    let processOperation (state: TreeTestState) operation: TreeTestState =
        match state with
        | Ok okState -> 
            let newState list tree =
                {
                    List = list; Tree = tree
                    OperationsPerformed = okState.OperationsPerformed + 1
                } |> Ok 

            let list = okState.List
            let tree = okState.Tree
                    
            match operation with
            | Insert value ->
                let list' = (value :: list) |> List.sort
                let tree' = tree |> insert value
                
                newState list' tree'
            | Remove vector ->
                let itemIndex =
                    ((list.Length |> float) - 1.) * vector
                    |> System.Math.Round |> int
                if itemIndex >= 0 && itemIndex < (list.Length - 1) then
                    let valueToRemove = list.[itemIndex]
                    let list' = list |> removeAt itemIndex
                    let tree' = tree |> remove valueToRemove
                    newState list' tree'
                else
                    newState list tree
            | TryRemove value ->
                list
                |> List.tryFindIndex (fun x -> x = value)
                |> function
                | Some itemIndex ->
                    let list' = list |> removeAt itemIndex
                    let tree' = tree |> tryRemove value
                    newState list' tree'
                | None -> newState list tree
        | Error _ -> state
            
    let checkOperationResults (state: TreeTestState): TreeTestState =
        match state with
        | Ok okState ->
            let list = okState.List
            let tree = okState.Tree

            let treeElements = tree |> items |> Seq.toList
            if treeElements <> list then okState |> Error
            else state
        | Error _ -> state    

    let foldFunc operation = (processOperation operation) >> checkOperationResults
        
    let initialState = { List = []; Tree = None; OperationsPerformed = 0 } |> Ok
    let finalState = operations |> Seq.fold foldFunc initialState
     
    match finalState with
    | Ok finalStateOk ->
        let finalTreeElements = finalStateOk.Tree |> items |> Seq.toList   
        (finalTreeElements = finalStateOk.List)
        |> Prop.label "the final tree does not have expected elements"
        |@ sprintf "%A <> %A" finalTreeElements finalStateOk.List
    | Error errorState -> 
        let errorTreeElements = errorState.Tree |> items |> Seq.toList   
        (errorTreeElements = errorState.List)
        |> Prop.label "the intermediate tree does not have expected elements"
        |@ sprintf
               "after %d operations, %A <> %A, tree:\n%s"
               errorState.OperationsPerformed errorTreeElements errorState.List
               (errorState.Tree |> treeToDot)

type BinarySearchTreePropertyTest
    (output: Xunit.Abstractions.ITestOutputHelper) =
    [<Fact>]
    member this.``Binary search tree property test``() =
        let genInsert = Arb.generate<int> |> Gen.map Insert
        let genRemove = (floatFrom0To1Inclusive 1000) |> Gen.map Remove
        let genTryRemove = Arb.generate<int> |> Gen.map TryRemove
        
        let genOperation = Gen.frequency [
            (5, genInsert); (1, genRemove); (1, genTryRemove)
        ]
        
        let gen = Gen.arrayOf genOperation
        
        ``binary search tree properties``
        |> checkPropertyWithTestSize gen output 200 100
//        |> replayPropertyCheck gen output (1113741011,296681894)
        
    [<Fact>]
    member this.``Bug case 1``() =
        let prop =
            ``binary search tree properties`` [
                Insert 0; Insert 4; Insert 0; Insert -1; TryRemove 0
            ]
        
        prop |> Check.VerboseThrowOnFailure