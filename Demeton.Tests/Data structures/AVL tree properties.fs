module Tests.``Data structures``.``AVL tree properties``

open DataStructures
open Tests.``Data structures``.``Binary search tree testbed``

/// Determines whether the tree is balanced (in terms of AVL tree balance
/// or not). 
let rec private isBalanced (tree: AvlTree.Tree<'T>) =
    match tree with
    | AvlTree.None -> true
    | AvlTree.Node node ->
        if node.Left |> isBalanced |> not then
            false
        elif node.Right |> isBalanced |> not then
            false
        else
            let leftHeight = node.Left |> AvlTree.height
            let rightHeight = node.Right |> AvlTree.height
            abs (leftHeight - rightHeight) <= 1

let avlTreeIsBalanced (state: TreeTestCurrent<AvlTree.Tree<'T>>) =
    if state.Tree |> isBalanced then state |> Ok
    else (state, "The tree is not balanced") |> Error
