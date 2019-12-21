/// Implementation of a persistent AVL tree.
///
/// Useful links:
/// - https://en.wikipedia.org/wiki/AVL_tree
/// - https://en.wikibooks.org/wiki/F_Sharp_Programming/Advanced_Data_Structures#AVL_Trees
/// - https://two-wrongs.com/purely-functional-avl-trees-in-common-lisp
[<RequireQualifiedAccess>]
module DataStructures.AvlTree

open DataStructures
open System.Collections.Generic
open Text

/// A node of the binary search tree.
type Node<'T when 'T:comparison> = {
    Item: 'T
    Left: Node<'T> option
    Right: Node<'T> option
}

/// The root of the binary search tree.
type Tree<'T when 'T:comparison> = Node<'T> option

/// Returns the height of the tree.
let rec height tree =
    match tree with
    | None -> 0
    | Some node ->
        max (node.Left |> height) (node.Right |> height) + 1

/// Contains internal implementation for inserting and removing of items from
/// the tree.
[<RequireQualifiedAccess>]
module private Node =
    /// Represents a subtree (basically just an optional node representing the
    /// root of the subtree. 
    type Subtree<'T when 'T:comparison> = Node<'T> option
    
    /// Creates a leaf node.
    let createLeaf item = { Item = item; Left = None; Right = None }
    
    /// Creates a node.
    let create item left right = { Item = item; Left = left; Right = right }
    
    /// Creates a copy of the node with a different left child.
    let updateLeft leftNode node = { node with Left = leftNode }
    /// Creates a copy of the node with a different right child.
    let updateRight rightNode node = { node with Right = rightNode }
    
    /// Determines whether the tree contains the specified item. 
    let rec contains item node =
        if item = node.Item then true
        elif item < node.Item then
            match node.Left with
            | Some left -> left |> contains item
            | None -> false
        else
            match node.Right with
            | Some right -> right |> contains item
            | None -> false
    
    /// Looks for the successor (the leftest descendant) node and returns its item and a
    /// replacement node (recursively).
    let rec private removeSuccessor (node: Node<'T>): 'T * Subtree<'T> =
        match node.Left with
        // if we found the successor node, save its item and give its right child
        // to the successor parent
        | None -> (node.Item, node.Right)
        // if there are still some nodes to the left...
        | Some left ->
            // find the successor and the new left child
            let (successorNodeItem, newLeftChild) = left |> removeSuccessor
            let node' = node |> updateLeft newLeftChild |> Some
            (successorNodeItem, node')
    
    /// Replaces (or, better, put, creates a new node of) the successor node of
    /// the right child.
    let private replaceWithSuccessor node =
        match node.Right with
        | None ->
            invalidOp
                "bug: this function should not be called on a node without the right child"
        | Some right -> 
            let (successorNodeItem, newRightChild) =
                right |> removeSuccessor
            create successorNodeItem node.Left newRightChild |> Some

    /// Removes an item from the subtree. If the item was not found,
    /// throws an exception.
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
                node |> updateLeft left' |> Some
        else
            match node.Right with
            | None -> None
            | Some rightNode ->
                let right' = rightNode |> remove item
                node |> updateRight right' |> Some
    
    /// The result type of the tryRemove function.
    type TryRemoveResult<'T when 'T:comparison> =
        /// The item was found in the specific subtree.
        | Found of Subtree<'T>
        /// The item was not found.
        | NotFound
    
    /// Tries to remove an item from the subtree. If the item was not found,
    /// just returns NotFound result.
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
                    node |> updateLeft newLeftNode |> Some |> Found
                | NotFound -> NotFound
        else
            match node.Right with
            | None -> NotFound
            | Some rightNode ->
                match tryRemove item rightNode with
                | Found newRightNode ->
                    node |> updateRight newRightNode |> Some |> Found
                | NotFound -> NotFound

    /// Outputs node ID (and any other attributes) in DOT language.
    let nodeToDot (node: Tree<'T>) (nodeCounter: int) output =
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

    /// Outputs the subtree in DOT language. 
    let rec subtreeToDot
        (node: Subtree<'T>) nodeId (nodeCounter: int) output: int =
      
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

    let balanceFactor node =
        match node with
        | None -> 0
        | Some node -> (height node.Left) - (height node.Right)
    
    let rotateLeft (node: Tree<'T>): Tree<'T> =
        match node with
        | Some { Left = left; Right = Some { Left = rl; Right = rr; Item = ri }
                 Item = item } ->
            let left' = create item left rl |> Some
            create ri left' rr |> Some
        | node -> node

    let rotateRight (node: Tree<'T>): Tree<'T> =
        match node with
        | Some { Left = Some { Left = ll; Right = lr; Item = li }
                 Right = right; Item = item } ->
            let right' = create item lr right |> Some
            create li ll right' |> Some
        | node -> node
    
    let doubleRotateLeft (node: Tree<'T>): Tree<'T> =
        match node with
        | Some node ->
            let right': Node<'T> option = node.Right |> rotateRight
            let node' = node |> updateRight right' |> Some
            node' |> rotateLeft
        | node -> node
        
    let doubleRotateRight (node: Tree<'T>): Tree<'T> =
        match node with
        | Some node ->
            let left' = node.Left |> rotateLeft
            let node' = node |> updateLeft left' |> Some
            node' |> rotateRight
        | node -> node        
    
    let balance (node: Tree<'T>): Tree<'T>  =
        match node with
        | Some n when node |> balanceFactor >= 2 ->
            if n.Left |> balanceFactor >= 1 then node |> rotateRight
            else node |> doubleRotateRight
        | Some n when node |> balanceFactor <= -2 ->
            if n.Right |> balanceFactor <= -1 then node |> rotateLeft 
            else node |> doubleRotateLeft
        | node -> node

/// Inserts an item into the tree and returns a new version of the tree.
let rec insert item (tree: Tree<'T>) =
    match tree with
    | None -> Node.createLeaf item |> Some
    | Some node ->
        let node' =
            if item < node.Item then
                let left' = insert item node.Left
                node |> Node.updateLeft left' |> Some
            else
                let right' = insert item node.Right
                node |> Node.updateRight right' |> Some
        Node.balance node'
   
/// Removes an item from the tree and returns a new version of the tree.
/// If the item was not found, throws an exception.
let remove item tree =
    match tree with
    | None ->
        KeyNotFoundException "The item was not found in the tree."
        |> raise
    | Some rootNode -> rootNode |> Node.remove item

/// Tries to remove an item from the tree. If the item was found and removed,
/// returns a new version of the tree. If the item was not found, returns the
/// original tree.
let tryRemove item tree =
    match tree with
    | None -> None
    | Some rootNode ->
        rootNode |> Node.tryRemove item
        |> function
        | Node.Found newTree -> newTree
        | Node.NotFound -> tree
        
let contains item tree =
    match tree with
    | None -> false
    | Some rootNode -> rootNode |> Node.contains item
        
/// Returns a sequence containing all of the items in the tree, sorted by
/// item keys. 
let rec items tree =
    match tree with
    | None -> seq []
    | Some node ->
        let leftItems = node.Left |> items
        let rightItems = node.Right |> items
        rightItems
        |> Seq.append [ node.Item ]
        |> Seq.append leftItems

/// Serializes the tree into string using the DOT language.
let treeToDot (tree: Tree<'T>) =
    match tree with
    | None -> ""
    | Some _ ->
        let builder =
            buildString()
            |> appendLine "digraph BST {"
        
        let nodeId = builder |> Node.nodeToDot tree 0
        builder |> Node.subtreeToDot tree nodeId 0 |> ignore
        
        builder
        |> appendLine "}"
        |> toString
