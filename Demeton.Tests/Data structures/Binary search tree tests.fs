module Tests.``Data structures``.``Binary search tree tests``

open DataStructures.ListEx

open DataStructures
open Xunit
open Swensen.Unquote
open FsCheck
open PropertiesHelp
open TestHelp

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
    Result<TreeTestCurrent<'Tree>, TreeTestCurrent<'Tree> * string>

type TreePropertyVerificationFunc<'Tree> =
    TreeTestCurrent<'Tree> -> TreeTestResult<'Tree>

/// Checks whether the current tree corresponds to its test oracle.
let verifyWithTestOracle items: TreePropertyVerificationFunc<'Tree> =
    fun (state: TreeTestCurrent<'Tree>) ->
    let list = state.List
    let tree = state.Tree

    let treeElements = tree |> items |> Seq.toList
    if treeElements <> list then
        (state, "The tree no longer corresponds to the test oracle")
        |> Error
    else state |> Ok

let rootIsBlack (state: TreeTestCurrent<RedBlackTree.Tree<'T>>) =
    if state.Tree |> RedBlackTree.isBlack then state |> Ok
    else (state, "Root node should be black") |> Error

let redNodesDoNotHaveRedChildren
    (state: TreeTestCurrent<RedBlackTree.Tree<'T>>) =
    let rec allNodesSatisfy (node: RedBlackTree.Tree<'T>) =
        match node with
        | None -> true
        | Some node ->
            match node.Color with
            | RedBlackTree.Black ->
                (node.Left |> allNodesSatisfy) &&
                    (node.Right |> allNodesSatisfy)
            | RedBlackTree.Red ->
                (node.Left |> RedBlackTree.isBlack)
                && (node.Right |> RedBlackTree.isBlack)
                && (node.Left |> allNodesSatisfy)
                && (node.Right |> allNodesSatisfy)
    
    if state.Tree |> allNodesSatisfy then state |> Ok
    else (state, "One or more red nodes has red children") |> Error
    
let rec countNumberOfBlacksInPath (tree: RedBlackTree.Tree<'T>) =
    match tree with
    | None -> (0, 0)
    | Some tree ->
        let (leftMin, leftMax) = tree.Left |> countNumberOfBlacksInPath
        let (rightMin, rightMax) = tree.Right |> countNumberOfBlacksInPath
        match tree.Color with
        | RedBlackTree.Red -> (min leftMin rightMin, max leftMax rightMax)
        | RedBlackTree.Black -> (1 + min leftMin rightMin, 1 + max leftMax rightMax)
    
let allPathsHaveSameNumberOfBlacks
    (state: TreeTestCurrent<RedBlackTree.Tree<'T>>) =

    let (minBlacks, maxBlacks) = countNumberOfBlacksInPath state.Tree
    if minBlacks = maxBlacks then state |> Ok
    else (state,
          sprintf
              "Paths have different number of blacks (from %d to %d)"
              minBlacks maxBlacks)
        |> Error

let private ``binary search tree properties``
    items contains insert remove tryRemove treeToDot
    (additionalProperties: TreePropertyVerificationFunc<'Tree> list)
    initialState operations =
    /// Executes a test operation on the tree.
    let processOperation
        (state: TreeTestCurrent<'Tree>) operation: TreeTestResult<'Tree> =
        let newState list tree =
            {
                List = list; Tree = tree
                OperationsPerformed = state.OperationsPerformed + 1
            } |> Ok 

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
                (state,
                    sprintf
                        "The contains function returned a wrong result for item %d"
                        item)
                |> Error
            
    let foldFunc
        (verifyProperties: TreePropertyVerificationFunc<'Tree> list) 
        (resultState: TreeTestResult<'Tree>)
        operation: TreeTestResult<'Tree> =
            
        let operationResult =
            match resultState with
            | Ok state -> processOperation state operation
            | Error error -> Error error
            
        verifyProperties
        |> List.fold (fun result property ->
            match result with
            | Ok state -> property state
            | Error error -> Error error)
            operationResult
    
    let classifyByTreeSize size property =
        property
        |> Prop.classify (size <= 5) "final tree size <= 5 elements"
        |> Prop.classify
               (size > 5 && size <= 10) "final tree size 6-10 elements"
        |> Prop.classify
               (size > 10 && size <= 20) "final tree size 11-20 elements"
        |> Prop.classify (size > 20) "final tree size >= 20 elements"
        
    let finalState =
        operations
        |> Seq.fold (foldFunc additionalProperties) initialState
     
    match finalState with
    | Result.Ok finalStateOk ->
        let finalTreeElements = finalStateOk.Tree |> items |> Seq.toList   
        (finalTreeElements = finalStateOk.List)
        |> classifyByTreeSize (finalStateOk.List |> List.length)
        |> Prop.label "the final tree does not have expected elements"
        |@ sprintf "%A <> %A" finalTreeElements finalStateOk.List
    | Result.Error (errorState, message) -> 
        let errorTreeElements = errorState.Tree |> items |> Seq.toList   
        false
        |> Prop.label message
        |@ sprintf
               "after %d operations, tree=%A, oracle=%A, tree:\n\n%s"
               errorState.OperationsPerformed errorTreeElements errorState.List
               (errorState.Tree |> treeToDot)

type BinarySearchTreePropertyTest
    (output: Xunit.Abstractions.ITestOutputHelper) =
    let unbalancedBinarySearchTreeProperties =
        let initialState =
            { List = []; Tree = None; OperationsPerformed = 0 } |> Ok
        
        (``binary search tree properties``
            UnbalancedBinarySearchTree.items
            UnbalancedBinarySearchTree.contains
            UnbalancedBinarySearchTree.insert
            UnbalancedBinarySearchTree.remove
            UnbalancedBinarySearchTree.tryRemove
            UnbalancedBinarySearchTree.treeToDot
            [ (verifyWithTestOracle UnbalancedBinarySearchTree.items) ]
            initialState)
    
    let redBlackTreeProperties =
        let initialState =
            { List = []; Tree = None; OperationsPerformed = 0 } |> Ok
        
        (``binary search tree properties``
            RedBlackTree.items
            RedBlackTree.contains
            RedBlackTree.insert
            RedBlackTree.remove
            RedBlackTree.tryRemove
            RedBlackTree.treeToDot
            [ (verifyWithTestOracle RedBlackTree.items)
              rootIsBlack
              redNodesDoNotHaveRedChildren
              allPathsHaveSameNumberOfBlacks ]
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
//        |> checkPropertyWithTestSize gen output 200 100
//        |> checkPropertyVerboseWithTestSize gen output 200 100
        |> replayPropertyCheck gen output (9157400,296683675)
   
    [<Fact>]
    member this.``Unbalanced binary search tree properties``() =
        runTreePropertyTests unbalancedBinarySearchTreeProperties
    
    [<Fact (Skip="todo")>]
    member this.``Red-black tree properties``() =
        runTreePropertyTests redBlackTreeProperties

    [<Fact>]
    member this.``Sample case 1``() =
        redBlackTreeProperties
            [| Insert -1; Insert 1; Insert -1; Insert 0; Insert 0 |]
        |> Check.QuickThrowOnFailure

    [<Fact>]
    member this.``Sample case 2``() =
        redBlackTreeProperties
            [| Insert 0; Insert 0; Insert 0; Insert -3 |]
        |> Check.QuickThrowOnFailure

    [<Fact>]
    member this.``Sample case 3``() =
        redBlackTreeProperties
            [| Insert 1; Insert 0; Contains 1; Insert -1
               Insert -1; Contains 0; Insert 2; Insert 0 |]
        |> Check.QuickThrowOnFailure

    [<Fact (Skip="todo")>]
    member this.``Sample case 4``() =
        redBlackTreeProperties
            [| Insert -1; Insert 0; Insert 0; Insert -1; Insert 1; Insert -1; Insert 1;
              Insert 1; Insert -1; Insert 2; Insert 0; Insert 1; Insert 0 |]
        |> Check.QuickThrowOnFailure
