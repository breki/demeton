module Tests.``Data structures``.``Binary search tree tests``

open Demeton.DataStructures.BinarySearchTree

open Xunit
open FsCheck
open PropertiesHelp
open Text

let private removeAt itemIndex list =
    let (left, right) = list |> List.splitAt itemIndex
    List.append left (right |> List.tail)

let rec private isBalanced tree =
    match tree with
    | None -> true
    | Some node ->
        if node.Left |> isBalanced |> not then
            false
        elif node.Right |> isBalanced |> not then
            false
        else
            let leftHeight = node.Left |> height
            let rightHeight = node.Right |> height
            abs (leftHeight - rightHeight) <= 1

let private nodeToDot (node: BinarySearchTree<'T>) (nodeCounter: int) output =
    match node with
    | Some node ->
        let nodeId = sprintf "%d" nodeCounter
        output
        |> appendFormat "{0} [label={1}]" [| nodeId; node.Item |]
        |> newLine |> ignore        
        nodeId
    | None ->
        let nodeId = sprintf "null%d" nodeCounter
        output
        |> appendFormat "{0} [shape=point]" [| nodeId |]
        |> newLine |> ignore
        nodeId

let rec private subtreeToDot
    (node: BinarySearchTree<'T>) nodeId (nodeCounter: int) output: int =
  
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
                |> subtreeToDot node.Left leftNodeId (nodeCounterBeforeLeft + 1)

        let rightNodeId = nodeToDot (node.Right) nodeCounterBeforeRight output
        output
        |> appendFormat "{0} -> {1}" [| nodeId; rightNodeId |]
        |> newLine |> ignore

        let nodeCounterAfterRight =
            match node.Right with
            | None -> nodeCounterBeforeRight + 1
            | Some _ ->
                output
                |> subtreeToDot node.Right rightNodeId (nodeCounterBeforeRight + 1)

        nodeCounterAfterRight

let treeToDot (tree: BinarySearchTree<'T>) =
    match tree with
    | None -> ""
    | Some _ ->
        let builder =
            buildString()
            |> appendLine "digraph BST {"
        
        let nodeId = builder |> nodeToDot tree 0
        builder |> subtreeToDot tree nodeId 0 |> ignore
        
        builder
        |> appendLine "}"
        |> toString

type private TreeOperation =
    | Insert of int
    | Remove of float
    | TryRemove of int

type TreeTestCurrent = {
    List: int list
    Tree: BinarySearchTree<int>
    OperationsPerformed: int
}

type TreeTestResult =
    | Correct of TreeTestCurrent
    | Incorrect of TreeTestCurrent
    | Unbalanced of TreeTestCurrent

let private ``binary search tree properties`` operations =
    let processOperation (state: TreeTestResult) operation: TreeTestResult =
        match state with
        | Correct state -> 
            let newState list tree =
                {
                    List = list; Tree = tree
                    OperationsPerformed = state.OperationsPerformed + 1
                } |> Correct 

            let list = state.List
            let tree = state.Tree
                    
            match operation with
            | Insert item ->
                let list' = (item :: list) |> List.sort
                let tree' = tree |> insert item
                
                newState list' tree'
            | Remove vector ->
                let itemIndex =
                    ((list.Length |> float) - 1.) * vector
                    |> System.Math.Round |> int
                if itemIndex >= 0 && itemIndex < (list.Length - 1) then
                    let itemToRemove = list.[itemIndex]
                    let list' = list |> removeAt itemIndex
                    let tree' = tree |> remove itemToRemove
                    newState list' tree'
                else
                    newState list tree
            | TryRemove item ->
                list
                |> List.tryFindIndex (fun x -> x = item)
                |> function
                | Some itemIndex ->
                    let list' = list |> removeAt itemIndex
                    let tree' = tree |> tryRemove item
                    newState list' tree'
                | None -> newState list tree
        | Incorrect state -> Incorrect state
        | Unbalanced state -> Unbalanced state
            
    let checkOperationResults (state: TreeTestResult): TreeTestResult =
        match state with
        | Correct state ->
            let list = state.List
            let tree = state.Tree

            let treeElements = tree |> items |> Seq.toList
            if treeElements <> list then state |> Incorrect
            // todo: we do not check the balance for BST since it is not
            // maintaining balance
//            elif tree |> isBalanced |> not then state |> Unbalanced
            else state |> Correct
        | Incorrect state -> Incorrect state
        | Unbalanced state -> Unbalanced state

    let foldFunc operation = (processOperation operation) >> checkOperationResults
    
    let classifyByTreeSize size property =
        property
        |> Prop.classify (size <= 5) "final tree size <= 5 elements"
        |> Prop.classify (size > 5 && size <= 10) "final tree size 6-10 elements"
        |> Prop.classify (size > 10 && size <= 20) "final tree size 11-20 elements"
        |> Prop.classify (size > 20) "final tree size >= 20 elements"
        
    let initialState = { List = []; Tree = None; OperationsPerformed = 0 } |> Correct
    let finalState = operations |> Seq.fold foldFunc initialState
     
    match finalState with
    | Correct finalStateOk ->
        let finalTreeElements = finalStateOk.Tree |> items |> Seq.toList   
        (finalTreeElements = finalStateOk.List)
        |> classifyByTreeSize (finalStateOk.List |> List.length)
        |> Prop.label "the final tree does not have expected elements"
        |@ sprintf "%A <> %A" finalTreeElements finalStateOk.List
    | Incorrect errorState -> 
        let errorTreeElements = errorState.Tree |> items |> Seq.toList   
        false
        |> Prop.label "the intermediate tree does not have expected elements"
        |@ sprintf
               "after %d operations, %A <> %A, tree:\n%s"
               errorState.OperationsPerformed errorTreeElements errorState.List
               (errorState.Tree |> treeToDot)
    | Unbalanced errorState ->
        false
        |> Prop.label "the tree is unbalanced"
        |@ sprintf
               "after %d operations, tree:\n%s"
               errorState.OperationsPerformed  (errorState.Tree |> treeToDot)

type BinarySearchTreePropertyTest
    (output: Xunit.Abstractions.ITestOutputHelper) =
    [<Fact>]
    member this.``Binary search tree property test``() =
        let genInsert = Arb.generate<int> |> Gen.map Insert
        let genRemove = (floatFrom0To1Inclusive 1000) |> Gen.map Remove
        let genTryRemove = Arb.generate<int> |> Gen.map TryRemove
        
        let genOperation = Gen.frequency [
            (20, genInsert); (1, genRemove); (1, genTryRemove)
        ]
        
        let gen = Gen.arrayOf genOperation
        
        ``binary search tree properties``
        |> checkPropertyWithTestSize gen output 200 100
//        |> checkPropertyVerboseWithTestSize gen output 200 100
//        |> replayPropertyCheck gen output (109530125,296682337)
