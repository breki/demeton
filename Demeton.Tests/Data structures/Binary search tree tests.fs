﻿module Tests.``Data structures``.``Binary search tree tests``

open DataStructures.ListEx

open DataStructures
open Xunit
open FsCheck
open PropertiesHelp

/// Determines whether the tree is balanced (in terms of AVL tree balance
/// or not). 
let rec private isBalanced (tree: DataStructures.RedBlackTree.Tree<'T>) =
    match tree with
    | None -> true
    | Some node ->
        if node.Left |> isBalanced |> not then
            false
        elif node.Right |> isBalanced |> not then
            false
        else
            let leftHeight = node.Left |> RedBlackTree.height
            let rightHeight = node.Right |> RedBlackTree.height
            abs (leftHeight - rightHeight) <= 1

/// The operation to perform on the tree.
type private TreeOperation =
    /// Insert a new item into the tree.
    | Insert of int
    /// Remove an item from the tree. The float value from 0 to 1 is
    /// multiplied by the current size of the tree to calculate the index of
    /// the item to be removed. 
    | Remove of float
    /// Try to remove an item from the tree. There is no guarantee the item
    /// is present in the tree.
    | TryRemove of int
    | Contains of int 

/// The current state of the binary search tree property test.
type TreeTestCurrent<'Tree> = {
    /// The list which serves as a test oracle. The tree should contain the same
    /// items as this list (and in the same order).
    List: int list
    /// The tree under the test.
    Tree: 'Tree
    /// Count of the tree operations performed so far.
    OperationsPerformed: int
}

/// Represents both the intermediate and final results of the binary search tree
/// property test. 
type TreeTestResult<'Tree> =
    /// Currently the tree corresponds to the test oracle.
    | Correct of TreeTestCurrent<'Tree>
    /// The tree no longer corresponds to the test oracle. No further operations
    /// will be performed on it.
    | Incorrect of TreeTestCurrent<'Tree>
    /// The tree is not balanced (this is not used when testing BinarySearchTree
    /// since it is not maintaining balance).
    | Unbalanced of TreeTestCurrent<'Tree>
    | ContainsFailed of (int * TreeTestCurrent<'Tree>)

let private ``binary search tree properties``
    items contains insert remove tryRemove treeToDot
    initialState
    operations =
    /// Executes a test operation on the tree.
    let processOperation
        (state: TreeTestResult<'Tree>) operation: TreeTestResult<'Tree> =
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
            | Contains item ->
                if list
                   |> List.contains item = (tree |> contains item) then
                    newState list tree 
                else
                    ContainsFailed(item, state)
        | Incorrect state -> Incorrect state
        | Unbalanced state -> Unbalanced state
        | ContainsFailed state -> ContainsFailed state
            
    /// Checks whether the current tree corresponds to its test oracle.
    let checkOperationResults (state: TreeTestResult<'Tree>): TreeTestResult<'Tree> =
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
        | ContainsFailed state -> ContainsFailed state

    let foldFunc operation =
        (processOperation operation) >> checkOperationResults
    
    let classifyByTreeSize size property =
        property
        |> Prop.classify (size <= 5) "final tree size <= 5 elements"
        |> Prop.classify
               (size > 5 && size <= 10) "final tree size 6-10 elements"
        |> Prop.classify
               (size > 10 && size <= 20) "final tree size 11-20 elements"
        |> Prop.classify (size > 20) "final tree size >= 20 elements"
        
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
               errorState.OperationsPerformed
               (errorState.Tree |> treeToDot)
    | ContainsFailed (item, state) ->
        false
        |> Prop.label "contains function is incorrect"
        |@ sprintf
               "contains %d function is incorrect, tree:\n%s"
               item (state.Tree |> treeToDot)


type BinarySearchTreePropertyTest
    (output: Xunit.Abstractions.ITestOutputHelper) =
    let unbalancedBinarySearchTreeProperties =
        let initialState =
            { List = []; Tree = None; OperationsPerformed = 0 } |> Correct
        
        (``binary search tree properties``
            UnbalancedBinarySearchTree.items
            UnbalancedBinarySearchTree.contains
            UnbalancedBinarySearchTree.insert
            UnbalancedBinarySearchTree.remove
            UnbalancedBinarySearchTree.tryRemove
            UnbalancedBinarySearchTree.treeToDot
            initialState)
    
    let redBlackTreeProperties =
        let initialState =
            { List = []; Tree = None; OperationsPerformed = 0 } |> Correct
        
        (``binary search tree properties``
            RedBlackTree.items
            RedBlackTree.contains
            RedBlackTree.insert
            RedBlackTree.remove
            RedBlackTree.tryRemove
            RedBlackTree.treeToDot
            initialState)
    
    let runTreePropertyTests properties =
        let genInsert = Arb.generate<int> |> Gen.map Insert
        let genRemove = (floatFrom0To1Inclusive 1000) |> Gen.map Remove
        let genTryRemove = Arb.generate<int> |> Gen.map TryRemove
        let genContains = Arb.generate<int> |> Gen.map Contains
        
        let genOperation = Gen.frequency [
            (20, genInsert); (1, genRemove); (1, genTryRemove); (3, genContains)
        ]
        
        let gen = Gen.arrayOf genOperation
        
        properties
        |> checkPropertyWithTestSize gen output 200 100
//        |> checkPropertyVerboseWithTestSize gen output 200 100
//        |> replayPropertyCheck gen output (109530125,296682337)
    
    [<Fact>]
    member this.``Unbalanced binary search tree properties``() =
        runTreePropertyTests unbalancedBinarySearchTreeProperties
    
    [<Fact>]
    member this.``Red-black tree properties``() =
        runTreePropertyTests redBlackTreeProperties