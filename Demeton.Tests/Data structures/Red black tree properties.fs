module Tests.``Data structures``.``Red black tree properties``

open DataStructures
open Tests.``Data structures``.``Binary search tree testbed``

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

