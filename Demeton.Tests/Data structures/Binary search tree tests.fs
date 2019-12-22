module Tests.``Data structures``.``Binary search tree tests``

open DataStructures.ListEx

open DataStructures
open Tests.``Data structures``.``Binary search tree testbed``
open Tests.``Data structures``.``AVL tree properties``
open Tests.``Data structures``.``Red black tree properties``
open Xunit
open FsCheck
open FsUnit
open PropertiesHelp

let log (output: Xunit.Abstractions.ITestOutputHelper) depth message =
    output.WriteLine message

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

let private ``binary search tree properties``
    (output: Xunit.Abstractions.ITestOutputHelper)
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
            
        let result =
            verifyProperties
            |> List.fold (fun result property ->
                match result with
                | Ok state -> property state
                | Error error -> Error error)
                operationResult
                
        match result with
        | Ok state ->
            output.WriteLine (state.Tree |> treeToDot) |> ignore
        | Error (state, _) ->
            output.WriteLine (state.Tree |> treeToDot) |> ignore
        
        result
    
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
            { List = []; Tree = UnbalancedBinarySearchTree.None
              OperationsPerformed = 0 } |> Ok
        
        (``binary search tree properties``
            output
            UnbalancedBinarySearchTree.items
            UnbalancedBinarySearchTree.contains
            UnbalancedBinarySearchTree.insert
            UnbalancedBinarySearchTree.remove
            UnbalancedBinarySearchTree.tryRemove
            UnbalancedBinarySearchTree.treeToDot
            [ (verifyWithTestOracle UnbalancedBinarySearchTree.items) ]
            initialState)

    let avlTreeProperties =
        let initialState =
            { List = []; Tree = AvlTree.None; OperationsPerformed = 0 } |> Ok
        
        (``binary search tree properties``
            output
            AvlTree.items
            AvlTree.contains
            AvlTree.insert
            AvlTree.remove
            AvlTree.tryRemove
            AvlTree.treeToDot
            [ (verifyWithTestOracle AvlTree.items)
              avlTreeIsBalanced ]
            initialState)
    
    let redBlackTreeProperties =
        let initialState =
            { List = []; Tree = RedBlackTree.None; OperationsPerformed = 0 }
            |> Ok
        
        (``binary search tree properties``
            output
            RedBlackTree.items
            RedBlackTree.contains
            (RedBlackTree.insert (log output))
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
        |> checkPropertyWithTestSize gen output 200 100
//        |> checkPropertyVerboseWithTestSize gen output 200 100
//        |> replayPropertyCheck gen output (9157400,296683675)
    
    let runInsertOnlyTreePropertyTests properties =
        let genInsert = Arb.generate<int> |> Gen.map Insert
        let genContains = Arb.generate<int> |> Gen.map Contains
        
        let genOperation = Gen.frequency [
            (20, genInsert); (3, genContains)
        ]
        
        let gen = Gen.arrayOf genOperation
        
        properties
        |> checkPropertyWithTestSize gen output 200 100
//        |> checkPropertyVerboseWithTestSize gen output 200 100
//        |> replayPropertyCheck gen output (9157400,296683675)
   
    [<Fact>]
    member this.``Unbalanced binary search tree properties``() =
        runTreePropertyTests unbalancedBinarySearchTreeProperties
    
    [<Fact (Skip="todo")>]
//    [<Fact>]
    member this.``AVL tree properties``() =
        runTreePropertyTests avlTreeProperties
    
    [<Fact (Skip="todo")>]
//    [<Fact>]
    member this.``Red-black tree properties``() =
        runTreePropertyTests redBlackTreeProperties

    [<Fact>]
    member this.``AVL sample case 1``() =
        avlTreeProperties
            [| Insert -1; Insert 0; Insert 1; Insert 0; Insert 0; Insert 0; Insert 0;
                Insert 0; Contains 0; Insert 0; Remove 0.182; Insert 0; Insert -3 |]
        |> Check.QuickThrowOnFailure

    [<Fact>]
    member this.``AVL sample case 2``() =
        avlTreeProperties
            [| Insert 0; Insert 0; Insert -1; Insert -1;
              Insert 0; Insert 0; Insert 0; Insert 0;
              TryRemove -1 |]
        |> Check.QuickThrowOnFailure

    [<Fact>]
    member this.``AVL sample case 3``() =
        avlTreeProperties
            [| Insert 0; Insert 0; Insert -9; Insert 0
               Remove 0.134; Insert 0 |]
        |> Check.QuickThrowOnFailure

    [<Fact (Skip="todo")>]
//    [<Fact>]
    member this.``AVL sample case 4``() =
        avlTreeProperties
            [| Insert 0; Insert 0; Insert 0;
                Insert 0; Insert 0; Insert 0; Insert 0; Insert 0; 
              Insert 8; Insert 0; Insert 0; Insert 0; Insert 0;
              Insert 0; Remove 0.377; Insert 0; Insert 0; Insert 20; Insert 0;
              TryRemove 0 |]
        |> Check.QuickThrowOnFailure
        
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

    [<Fact>]
    member this.``Sample case 4``() =
        redBlackTreeProperties
            [| Insert -1; Insert 0; Insert 0; Insert -1; Insert 1; Insert -1; Insert 1;
              Insert 1; Insert -1; Insert 2; Insert 0; Insert 1; Insert 0 |]
        |> Check.QuickThrowOnFailure

    [<Fact>]
    member this.``Sample case 5``() =
        redBlackTreeProperties
            [| Insert 0; Insert 0; Contains 0; Contains 16; Insert 0; Insert 0; Contains 0;
              Insert 0; Insert 0; Insert 0; Insert 0; Insert 0; Contains 0; Insert 0;
              Insert 0 |]
        |> Check.QuickThrowOnFailure

    [<Fact (Skip="todo")>]
//    [<Fact>]
    member this.``Sample case 6``() =
        redBlackTreeProperties
            [| Insert 0; Insert 4; Insert 0; Insert -3; Insert 1; Insert 0;
                Insert 0; Insert 0; Insert 0 |]
        |> Check.QuickThrowOnFailure
