/// Implementation of a persistent balanced binary search tree (BST).
///
/// Useful links:
/// - https://en.wikipedia.org/wiki/Binary_search_tree
/// - https://www.geeksforgeeks.org/binary-search-tree-data-structure/
/// - https://en.wikipedia.org/wiki/Persistent_data_structure#Trees

module Demeton.DataStructures.BinarySearchTree

open System.Collections.Generic

type Node = {
    Value: int
    Left: Node option
    Right: Node option
}

type BinarySearchTree = Node option


[<RequireQualifiedAccess>]
module private Node =
    type Subtree = Node option
    
    let createLeaf value = { Value = value; Left = None; Right = None }
    let create value left right = { Value = value; Left = left; Right = right }
    
    let updateLeft leftNode node = { node with Left = Some leftNode }
    let updateLeftOption leftNode node = { node with Left = leftNode }
    let updateRight rightNode node = { node with Right = Some rightNode }
    let updateRightOption rightNode node = { node with Right = rightNode }
    
    let rec insert value node =
        if value < node.Value then
            let leftNode' = 
                match node.Left with
                | None -> createLeaf value |> Some
                | Some leftNode -> leftNode |> insert value
            node |> updateLeftOption leftNode' |> Some
        else
            let rightNode' =
                match node.Right with
                | None -> createLeaf value |> Some
                | Some rightNode -> rightNode |> insert value
            node |> updateRightOption rightNode' |> Some

    let rec private removeSuccessor (node: Node): int * Subtree =
        match node.Left with
        // if we found the successor node, save its value and give its right child
        // to the successor parent
        | None -> (node.Value, node.Right)
        // if there are still some nodes to the left...
        | Some left ->
            // find the successor and the new left child
            let (successorNodeValue, newLeftChild) = left |> removeSuccessor
            let node' = node |> updateLeftOption newLeftChild |> Some
            (successorNodeValue, node')
    
    let private replaceWithSuccessor node =
        match node.Right with
        | None ->
            invalidOp
                "bug: this function should not be called on a node without the right child"
        | Some right -> 
            let (successorNodeValue, newRightChild) =
                right |> removeSuccessor
            create successorNodeValue node.Left newRightChild |> Some

    let rec remove value node =
        if value = node.Value then
            match node.Left, node.Right with
            | None, None -> None
            | Some left, None -> Some left
            | None, Some right -> Some right
            | Some _, Some _ -> node |> replaceWithSuccessor
        elif value < node.Value then
            match node.Left with
            | None -> None
            | Some leftNode ->
                let left' = leftNode |> remove value
                node |> updateLeftOption left' |> Some
        else
            match node.Right with
            | None -> None
            | Some rightNode ->
                let right' = rightNode |> remove value
                node |> updateRightOption right' |> Some
    
    type TryRemoveResult =
        | Found of Subtree
        | NotFound
    
    let rec tryRemove value node: TryRemoveResult =
        if value = node.Value then
            match node.Left, node.Right with
            | None, None -> Found None
            | Some left, None -> Some left |> Found
            | None, Some right -> Some right |> Found
            | Some _, Some _ -> node |> replaceWithSuccessor |> Found
        elif value < node.Value then
            match node.Left with
            | None -> NotFound
            | Some leftNode ->
                match tryRemove value leftNode with
                | Found newLeftNode ->
                    node |> updateLeftOption newLeftNode |> Some |> Found
                | NotFound -> NotFound
        else
            match node.Right with
            | None -> NotFound
            | Some rightNode ->
                match tryRemove value rightNode with
                | Found newRightNode ->
                    node |> updateRightOption newRightNode |> Some |> Found
                | NotFound -> NotFound

let insert value tree =
    match tree with
    | None -> Node.createLeaf value |> Some
    | Some rootNode -> rootNode |> Node.insert value
    
let remove value tree =
    match tree with
    | None ->
        KeyNotFoundException "The value was not found in the tree."
        |> raise
    | Some rootNode -> rootNode |> Node.remove value

let tryRemove value tree =
    match tree with
    | None -> None
    | Some rootNode ->
        rootNode |> Node.tryRemove value
        |> function
        | Node.Found newTree -> newTree
        | Node.NotFound -> tree
        
let rec items tree =
    match tree with
    | None -> seq []
    | Some node ->
        let leftItems = node.Left |> items
        let rightItems = node.Right |> items
        rightItems
        |> Seq.append [ node.Value ]
        |> Seq.append leftItems
