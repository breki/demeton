/// Implementation of a persistent AVL tree.
///
/// Useful links:
/// - https://en.wikipedia.org/wiki/AVL_tree
/// - https://en.wikibooks.org/wiki/F_Sharp_Programming/Advanced_Data_Structures#AVL_Trees
/// - https://two-wrongs.com/purely-functional-avl-trees-in-common-lisp
[<RequireQualifiedAccess>]
module DataStructures.AvlTree

open System.Collections.Generic
open Text

type LogFunc = string -> unit

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

let nodeItem = function
    | Node node -> node.Item
    | None -> invalidOp "Node is None"

/// Returns the height of the tree.
let rec height tree =
    match tree with
    | None -> 0
    | Node node ->
        max (node.Left |> height) (node.Right |> height) + 1

/// Contains internal implementation for inserting and removing of items from
/// the tree.
[<RequireQualifiedAccess>]
module private Node =  
    let balanceFactor node =
        match node with
        | None -> 0
        | Node node -> (height node.Left) - (height node.Right)

    /// Creates a leaf node.
    let createLeaf item = { Item = item; Left = None; Right = None } |> Node
    
    /// Creates a node.
    let create item left right =
        Node { Item = item; Left = left; Right = right }
    
    /// Creates a copy of the node with a different left child.
    let updateLeft leftNode node = Node { node with Left = leftNode }
    /// Creates a copy of the node with a different right child.
    let updateRight rightNode node = Node { node with Right = rightNode }
        
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

    let rotateLeft (log: LogFunc) (node: Tree<'T>): Tree<'T> =
        log (sprintf "rotateLeft %A" (node |> nodeItem))
        
        match node with
        | Node { Left = left; Right = Node { Left = rl; Right = rr; Item = ri }
                 Item = item } ->
            let left' = create item left rl
            create ri left' rr
        | node -> node

    let rotateRight (log: LogFunc) (node: Tree<'T>): Tree<'T> =
        log (sprintf "rotateRight %A" (node |> nodeItem))
        
        match node with
        | Node { Left = Node { Left = ll; Right = lr; Item = li }
                 Right = right; Item = item } ->
            let right' = create item lr right
            create li ll right'
        | node -> node
    
    let doubleRotateLeft (log: LogFunc) (node: Tree<'T>): Tree<'T> =
        log (sprintf "doubleRotateLeft %A" (node |> nodeItem))
        
        match node with
        | Node node ->
            let right': Tree<'T> = node.Right |> rotateRight log
            let node' = node |> updateRight right'
            node' |> rotateLeft log
        | node -> node
        
    let doubleRotateRight (log: LogFunc) (node: Tree<'T>): Tree<'T> =
        log (sprintf "doubleRotateRight %A" (node |> nodeItem))
        
        match node with
        | Node node ->
            let left' = node.Left |> rotateLeft log
            let node' = node |> updateLeft left'
            node' |> rotateRight log
        | node -> node        
    
    let balance (log: LogFunc) (node: Tree<'T>): Tree<'T>  =
//        log (sprintf "balance %A" (node |> nodeItem))
        
        match node with
        | Node n when node |> balanceFactor >= 2 ->
            if n.Left |> balanceFactor >= 1 then node |> rotateRight log
            else node |> doubleRotateRight log
        | Node n when node |> balanceFactor <= -2 ->
            if n.Right |> balanceFactor <= -1 then node |> rotateLeft log 
            else node |> doubleRotateLeft log
        | node -> node
    
    /// Looks for the successor (the leftest descendant) node and returns its item and a
    /// replacement node (recursively).
    let rec private removeSuccessor
        (log: LogFunc) (node: Node<'T>): 'T * Tree<'T> =

        log (sprintf "removeSuccessor %A" node.Item)

        match node.Left with
        // if we found the successor node, save its item and give its right child
        // to the successor parent
        | None -> (node.Item, node.Right)
        // if there are still some nodes to the left...
        | Node left ->
            // find the successor and the new left child
            let (successorNodeItem, newLeftChild) = left |> removeSuccessor log
            let node' =
                node
                |> updateLeft newLeftChild
                |> balance log
            
//            let balanceFactor = node' |> balanceFactor
//            if balanceFactor <= -2 || balanceFactor >= 2 then
//                invalidOp "todo: successor unbalanced"            
            
            (successorNodeItem, node')
    
    /// Replaces (or, better, put, creates a new node of) the successor node of
    /// the right child.
    let private replaceWithSuccessor (log: LogFunc) node =
        log (sprintf "replaceWithSuccessor %A" node.Item)

        match node.Right with
        | None ->
            invalidOp
                "bug: this function should not be called on a node without the right child"
        | Node right -> 
            let (successorNodeItem, newRightChild) =
                right |> removeSuccessor log
            create successorNodeItem node.Left newRightChild
            |> balance log

    /// Removes an item from the subtree. If the item was not found,
    /// throws an exception.
    let rec remove (log: LogFunc) item node =
        log (sprintf "remove item %A from %A" item node.Item)

        if item = node.Item then
            match node.Left, node.Right with
            | None, None -> None
            | Node left, None -> Node left
            | None, Node right -> Node right
            | Node _, Node _ ->
                node
                |> replaceWithSuccessor log
                |> balance log
        elif item < node.Item then
            match node.Left with
            | None -> None
            | Node leftNode ->
                let left' = leftNode |> remove log item
                node |> updateLeft left' |> balance log
        else
            match node.Right with
            | None -> None
            | Node rightNode ->
                let right' = rightNode |> remove log item
                node |> updateRight right' |> balance log
    
    /// The result type of the tryRemove function.
    type TryRemoveResult<'T when 'T:comparison> =
        /// The item was found in the specific subtree.
        | Found of Tree<'T>
        /// The item was not found.
        | NotFound
    
    /// Tries to remove an item from the subtree. If the item was not found,
    /// just returns NotFound result.
    let rec tryRemove (log: LogFunc) item node: TryRemoveResult<'T> =
        log (sprintf "tryRemove item %A from %A" item node)

        if item = node.Item then
            match node.Left, node.Right with
            | None, None -> Found None
            | Node left, None -> Node left |> Found
            | None, Node right -> Node right |> Found
            | Node _, Node _ ->
                node
                |> replaceWithSuccessor log
                |> balance log
                |> Found
        elif item < node.Item then
            match node.Left with
            | None -> NotFound
            | Node leftNode ->
                match tryRemove log item leftNode with
                | Found newLeftNode ->
                    node
                    |> updateLeft newLeftNode
                    |> balance log
                    |> Found
                | NotFound -> NotFound
        else
            match node.Right with
            | None -> NotFound
            | Node rightNode ->
                match tryRemove log item rightNode with
                | Found newRightNode ->
                    node
                    |> updateRight newRightNode
                    |> balance log
                    |> Found
                | NotFound -> NotFound

/// Inserts an item into the tree and returns a new version of the tree.
let rec insert (log: LogFunc) item (tree: Tree<'T>) =
    match tree with
    | None -> Node.createLeaf item
    | Node node ->
        let node' =
            if item < node.Item then
                let left' = insert log item node.Left
                node |> Node.updateLeft left'
            else
                let right' = insert log item node.Right
                node |> Node.updateRight right'
        Node.balance log node'
   
/// Removes an item from the tree and returns a new version of the tree.
/// If the item was not found, throws an exception.
let remove (log: LogFunc) item tree =
    log (sprintf "remove item %A" item)

    match tree with
    | None ->
        KeyNotFoundException "The item was not found in the tree."
        |> raise
    | Node rootNode -> rootNode |> Node.remove log item

/// Tries to remove an item from the tree. If the item was found and removed,
/// returns a new version of the tree. If the item was not found, returns the
/// original tree.
let tryRemove (log: LogFunc) item tree =
    log (sprintf "tryRemove item %A" item)

    match tree with
    | None -> None
    | Node rootNode ->
        rootNode |> Node.tryRemove log item
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

/// Serializes the tree into string using the DOT language.
let treeToDot (tree: Tree<'T>) =
    let isNode = function
        | Node _ -> true
        | None -> false
        
    let nodeAttributes = function
        | Node node -> sprintf "label=%A" node.Item
        | None -> ""
    
    let leftChild = function
        | Node ({Left = left}) -> left
        | _ -> None
    
    let rightChild = function
        | Node ({Right = right}) -> right
        | _ -> None
    
    tree
    |> DataStructures.BinaryTreeToDot.treeToDot
           isNode nodeAttributes leftChild rightChild
