/// Implementation of a persistent unbalanced binary search tree (BST).
///
/// Useful links:
/// - https://en.wikipedia.org/wiki/Binary_search_tree
/// - https://www.geeksforgeeks.org/binary-search-tree-data-structure/
/// - https://en.wikipedia.org/wiki/Persistent_data_structure#Trees
[<RequireQualifiedAccess>]
module DataStructures.UnbalancedBinarySearchTree

open System.Collections.Generic
open Text

/// A node of the binary search tree.
type Node<'T when 'T:comparison> = {
    Item: 'T
    Left: Tree<'T>
    Right: Tree<'T>
}
/// The root of the binary search tree.
and Tree<'T when 'T:comparison> =
    | Node of Node<'T>
    | None

/// Contains internal implementation for inserting and removing of items from
/// the tree.
[<RequireQualifiedAccess>]
module private Node =
    /// Creates a leaf node.
    let createLeaf item = { Item = item; Left = None; Right = None }
    
    /// Creates a node.
    let create item left right = { Item = item; Left = left; Right = right }
    
    /// Creates a copy of the node with a different left child.
    let updateLeft leftNode node = { node with Left = leftNode }
    /// Creates a copy of the node with a different right child.
    let updateRight rightNode node = { node with Right = rightNode }
    
    /// Inserts an item into the subtree).
    let rec insert item node =
        if item < node.Item then
            let leftNode' = 
                match node.Left with
                | None -> createLeaf item |> Node
                | Node leftNode -> leftNode |> insert item
            node |> updateLeft leftNode' |> Node
        else
            let rightNode' =
                match node.Right with
                | None -> createLeaf item |> Node
                | Node rightNode -> rightNode |> insert item
            node |> updateRight rightNode' |> Node

    /// Determines whether the tree contains the specified item. 
    let rec contains item node =
        if item = node.Item then true
        elif item < node.Item then
            match node.Left with
            | Node left -> left |> contains item
            | None -> false
        else
            match node.Right with
            | Node right -> right |> contains item
            | None -> false
    
    /// Looks for the successor (the leftest descendant) node and returns its item and a
    /// replacement node (recursively).
    let rec private removeSuccessor (node: Node<'T>): 'T * Tree<'T> =
        match node.Left with
        // if we found the successor node, save its item and give its right child
        // to the successor parent
        | None -> (node.Item, node.Right)
        // if there are still some nodes to the left...
        | Node left ->
            // find the successor and the new left child
            let (successorNodeItem, newLeftChild) = left |> removeSuccessor
            let node' = node |> updateLeft newLeftChild |> Node
            (successorNodeItem, node')
    
    /// Replaces (or, better, put, creates a new node of) the successor node of
    /// the right child.
    let private replaceWithSuccessor node =
        match node.Right with
        | None ->
            invalidOp
                "bug: this function should not be called on a node without the right child"
        | Node right -> 
            let (successorNodeItem, newRightChild) =
                right |> removeSuccessor
            create successorNodeItem node.Left newRightChild |> Node

    /// Removes an item from the subtree. If the item was not found,
    /// throws an exception.
    let rec remove item node =
        if item = node.Item then
            match node.Left, node.Right with
            | None, None -> None
            | Node left, None -> Node left
            | None, Node right -> Node right
            | Node _, Node _ -> node |> replaceWithSuccessor
        elif item < node.Item then
            match node.Left with
            | None -> None
            | Node leftNode ->
                let left' = leftNode |> remove item
                node |> updateLeft left' |> Node
        else
            match node.Right with
            | None -> None
            | Node rightNode ->
                let right' = rightNode |> remove item
                node |> updateRight right' |> Node
    
    /// The result type of the tryRemove function.
    type TryRemoveResult<'T when 'T:comparison> =
        /// The item was found in the specific subtree.
        | Found of Tree<'T>
        /// The item was not found.
        | NotFound
    
    /// Tries to remove an item from the subtree. If the item was not found,
    /// just returns NotFound result.
    let rec tryRemove item node: TryRemoveResult<'T> =
        if item = node.Item then
            match node.Left, node.Right with
            | None, None -> Found None
            | Node left, None -> Node left |> Found
            | None, Node right -> Node right |> Found
            | Node _, Node _ -> node |> replaceWithSuccessor |> Found
        elif item < node.Item then
            match node.Left with
            | None -> NotFound
            | Node leftNode ->
                match tryRemove item leftNode with
                | Found newLeftNode ->
                    node |> updateLeft newLeftNode |> Node |> Found
                | NotFound -> NotFound
        else
            match node.Right with
            | None -> NotFound
            | Node rightNode ->
                match tryRemove item rightNode with
                | Found newRightNode ->
                    node |> updateRight newRightNode |> Node |> Found
                | NotFound -> NotFound

    /// Outputs node ID (and any other attributes) in DOT language.
    let nodeToDot (node: Tree<'T>) (nodeCounter: int) output =
        match node with
        | Node node ->
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

    /// Outputs the subtree in DOT language. 
    let rec subtreeToDot
        (node: Tree<'T>) nodeId (nodeCounter: int) output: int =
      
        match node with
        | None -> nodeCounter
        | Node node ->
            let nodeCounterBeforeLeft = nodeCounter + 1
            
            let leftNodeId = nodeToDot (node.Left) nodeCounterBeforeLeft output

            output
            |> appendFormat "{0} -> {1}" [| nodeId; leftNodeId |]
            |> newLine |> ignore

            let nodeCounterBeforeRight =
                match node.Left with
                | None -> nodeCounterBeforeLeft + 1
                | Node _ ->
                    output
                    |> subtreeToDot node.Left leftNodeId (nodeCounterBeforeLeft + 1)

            let rightNodeId = nodeToDot (node.Right) nodeCounterBeforeRight output
            output
            |> appendFormat "{0} -> {1}" [| nodeId; rightNodeId |]
            |> newLine |> ignore

            let nodeCounterAfterRight =
                match node.Right with
                | None -> nodeCounterBeforeRight + 1
                | Node _ ->
                    output
                    |> subtreeToDot node.Right rightNodeId (nodeCounterBeforeRight + 1)

            nodeCounterAfterRight

/// Inserts an item into the tree and returns a new version of the tree.
let insert item (tree: Tree<'T>) =
    match tree with
    | None -> Node.createLeaf item |> Node
    | Node rootNode -> rootNode |> Node.insert item
   
/// Removes an item from the tree and returns a new version of the tree.
/// If the item was not found, throws an exception.
let remove item tree =
    match tree with
    | None ->
        KeyNotFoundException "The item was not found in the tree."
        |> raise
    | Node rootNode -> rootNode |> Node.remove item

/// Tries to remove an item from the tree. If the item was found and removed,
/// returns a new version of the tree. If the item was not found, returns the
/// original tree.
let tryRemove item tree =
    match tree with
    | None -> None
    | Node rootNode ->
        rootNode |> Node.tryRemove item
        |> function
        | Node.Found newTree -> newTree
        | Node.NotFound -> tree
        
let contains item tree =
    match tree with
    | None -> false
    | Node rootNode -> rootNode |> Node.contains item
        
/// Returns a sequence containing all of the items in the tree, sorted by
/// item keys. 
let rec items tree =
    match tree with
    | None -> seq []
    | Node node ->
        let leftItems = node.Left |> items
        let rightItems = node.Right |> items
        rightItems
        |> Seq.append [ node.Item ]
        |> Seq.append leftItems

/// Returns the height of the tree.
let rec height tree =
    match tree with
    | None -> 0
    | Node node ->
        max (node.Left |> height) (node.Right |> height) + 1

/// Serializes the tree into string using the DOT language.
let treeToDot (tree: Tree<'T>) =
    match tree with
    | None -> ""
    | Node _ ->
        let builder =
            buildString()
            |> appendLine "digraph BST {"
        
        let nodeId = builder |> Node.nodeToDot tree 0
        builder |> Node.subtreeToDot tree nodeId 0 |> ignore
        
        builder
        |> appendLine "}"
        |> toString
