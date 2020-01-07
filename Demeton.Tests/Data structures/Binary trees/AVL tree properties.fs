module Tests.``Data structures``.``Binary trees``.``AVL tree properties``

open DataStructures.BinaryTrees
open Tests.``Data structures``.``Binary trees``.``Binary search tree testbed``

/// Determines whether the tree is balanced (in terms of AVL tree balance
/// or not). 
let rec private isBalanced
    (treeFuncs: AvlTree.TreeFuncs<'TNode, 'TItem, 'TKey>)
    (tree: AvlTree.Tree<'TNode>) =
    /// Calculates the actual height of the AVL tree without relying on the
    /// Height property, so we can really verify the balance.
    let rec height tree =
        match tree with
        | AvlTree.None -> 0
        | AvlTree.Node node ->
            1 + max (node |> treeFuncs.Left |> height)
                    (node |> treeFuncs.Right |> height)
    
    match tree with
    | AvlTree.None -> true
    | AvlTree.Node node ->
        if node |> treeFuncs.Left |> isBalanced treeFuncs |> not then
            false
        elif node |> treeFuncs.Right |> isBalanced treeFuncs |> not then
            false
        else
            let leftHeight = node |> treeFuncs.Left |> height
            let rightHeight = node |> treeFuncs.Right |> height
            abs (leftHeight - rightHeight) <= 1

let avlTreeIsBalanced
    (treeFuncs: AvlTree.TreeFuncs<'TNode, 'TItem, 'TKey>)
    (state: TreeTestCurrent<AvlTree.Tree<'TNode>>) =
    if state.Tree |> isBalanced treeFuncs then state |> Ok
    else (state, "The tree is not balanced") |> Error
