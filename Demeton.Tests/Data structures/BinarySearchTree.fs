/// Implementation of a persistent unbalanced binary search tree (BST).
///
/// Useful links:
/// - https://en.wikipedia.org/wiki/Binary_search_tree
/// - https://www.geeksforgeeks.org/binary-search-tree-data-structure/
/// - https://en.wikipedia.org/wiki/Persistent_data_structure#Trees
module Demeton.DataStructures.BinarySearchTree

open System.Collections.Generic

/// A node of the binary search tree.
type Node<'T when 'T:comparison> = {
    Item: 'T
    Left: Node<'T> option
    Right: Node<'T> option
}

type BinarySearchTree<'T when 'T:comparison> = Node<'T> option

[<RequireQualifiedAccess>]
module private Node =
    type Subtree<'T when 'T:comparison> = Node<'T> option
    
    let createLeaf item = { Item = item; Left = None; Right = None }
    let create item left right = { Item = item; Left = left; Right = right }
    
    let updateLeft leftNode node = { node with Left = Some leftNode }
    let updateLeftOption leftNode node = { node with Left = leftNode }
    let updateRight rightNode node = { node with Right = Some rightNode }
    let updateRightOption rightNode node = { node with Right = rightNode }
    
    let rec insert item node =
        if item < node.Item then
            let leftNode' = 
                match node.Left with
                | None -> createLeaf item |> Some
                | Some leftNode -> leftNode |> insert item
            node |> updateLeftOption leftNode' |> Some
        else
            let rightNode' =
                match node.Right with
                | None -> createLeaf item |> Some
                | Some rightNode -> rightNode |> insert item
            node |> updateRightOption rightNode' |> Some

    let rec private removeSuccessor (node: Node<'T>): 'T * Subtree<'T> =
        match node.Left with
        // if we found the successor node, save its item and give its right child
        // to the successor parent
        | None -> (node.Item, node.Right)
        // if there are still some nodes to the left...
        | Some left ->
            // find the successor and the new left child
            let (successorNodeItem, newLeftChild) = left |> removeSuccessor
            let node' = node |> updateLeftOption newLeftChild |> Some
            (successorNodeItem, node')
    
    let private replaceWithSuccessor node =
        match node.Right with
        | None ->
            invalidOp
                "bug: this function should not be called on a node without the right child"
        | Some right -> 
            let (successorNodeItem, newRightChild) =
                right |> removeSuccessor
            create successorNodeItem node.Left newRightChild |> Some

    let rec remove item node =
        if item = node.Item then
            match node.Left, node.Right with
            | None, None -> None
            | Some left, None -> Some left
            | None, Some right -> Some right
            | Some _, Some _ -> node |> replaceWithSuccessor
        elif item < node.Item then
            match node.Left with
            | None -> None
            | Some leftNode ->
                let left' = leftNode |> remove item
                node |> updateLeftOption left' |> Some
        else
            match node.Right with
            | None -> None
            | Some rightNode ->
                let right' = rightNode |> remove item
                node |> updateRightOption right' |> Some
    
    type TryRemoveResult<'T when 'T:comparison> =
        | Found of Subtree<'T>
        | NotFound
    
    let rec tryRemove item node: TryRemoveResult<'T> =
        if item = node.Item then
            match node.Left, node.Right with
            | None, None -> Found None
            | Some left, None -> Some left |> Found
            | None, Some right -> Some right |> Found
            | Some _, Some _ -> node |> replaceWithSuccessor |> Found
        elif item < node.Item then
            match node.Left with
            | None -> NotFound
            | Some leftNode ->
                match tryRemove item leftNode with
                | Found newLeftNode ->
                    node |> updateLeftOption newLeftNode |> Some |> Found
                | NotFound -> NotFound
        else
            match node.Right with
            | None -> NotFound
            | Some rightNode ->
                match tryRemove item rightNode with
                | Found newRightNode ->
                    node |> updateRightOption newRightNode |> Some |> Found
                | NotFound -> NotFound

let insert item tree =
    match tree with
    | None -> Node.createLeaf item |> Some
    | Some rootNode -> rootNode |> Node.insert item
    
let remove item tree =
    match tree with
    | None ->
        KeyNotFoundException "The item was not found in the tree."
        |> raise
    | Some rootNode -> rootNode |> Node.remove item

let tryRemove item tree =
    match tree with
    | None -> None
    | Some rootNode ->
        rootNode |> Node.tryRemove item
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
        |> Seq.append [ node.Item ]
        |> Seq.append leftItems

let rec height tree =
    match tree with
    | None -> 0
    | Some node ->
        max (node.Left |> height) (node.Right |> height) + 1
