/// Implementation of a persistent red-black tree.
///
/// Useful links:
/// - https://en.wikipedia.org/wiki/Red%E2%80%93black_tree
/// - http://matt.might.net/articles/red-black-delete/
/// - https://www.cs.usfca.edu/~galles/visualization/RedBlack.html
[<RequireQualifiedAccess>]
module DataStructures.RedBlackTree

open System.Collections.Generic

type Color = Red | Black

/// A node of the binary search tree.
type Node<'T when 'T:comparison> = {
    Item: 'T
    Color: Color
    Left: Tree<'T>
    Right: Tree<'T>
}

/// The root of the binary search tree.
and Tree<'T when 'T:comparison> =
    | Node of Node<'T>
    | None

let isBlack (node: Tree<'T>) =
    match node with
    | None -> true
    | Node node ->
        match node.Color with
        | Black -> true
        | Red -> false

let isRed (node: Tree<'T>) = node |> isBlack |> not

let color (node: Tree<'T>) =
    if node |> isBlack then Black
    else Red

type LoggingFunc = int -> string -> unit

/// Serializes the tree into string using the DOT language.
let treeToDot (tree: Tree<'T>) =
    let isNode = function
        | Node _ -> true
        | None -> false
        
    let nodeAttributes = function
        | Node node ->
            sprintf
                "label=%A color=%s fontcolor=white style=filled"
                node.Item
                (if node.Color = Black then "black" else "red")
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

/// Contains internal implementation for inserting and removing of items from
/// the tree.
[<RequireQualifiedAccess>]
module private Node =
    let nodeId (node: Tree<'T>) =
        match node with
        | Node node -> sprintf "(%A)" node.Item
        | None -> "None"
    
    let nodeDesc (node: Tree<'T>) =
        match node with
        | Node node ->
            sprintf "(%A) L=(%A) R=(%A)"
                node.Item (node.Left |> nodeId) (node.Right |> nodeId)
        | None -> "None"
        
    type ParentRelation<'T when 'T:comparison> =
        | LeftChildOf of Node<'T>
        | RightChildOf of Node<'T>
        | NoParent
    
    type InsertNodeResult<'T when 'T:comparison> =
        | RepairParent of Tree<'T>
        | LeftRotate of Tree<'T>
        | RightRotate of Tree<'T>
        | RepaintAunt of Tree<'T>
        | RepaintGrandparentAndAunt of Tree<'T>
    
    let leftChildOf node = LeftChildOf node 
    let rightChildOf node = RightChildOf node 
    
    let aunt grandparent =
        match grandparent with
        | LeftChildOf grandparent -> grandparent.Right
        | RightChildOf grandparent -> grandparent.Left
        | NoParent -> None
    
    /// Creates a leaf node.
    let createLeaf item color =
        Node { Item = item; Color = color; Left = None; Right = None }
    
    /// Creates a node.
    let create item color left right =
        Node { Item = item; Color = color; Left = left; Right = right }
    
    /// Creates a copy of the node with a different left child.
    let updateLeft leftNode node = Node { node with Left = leftNode }
    
    let updateLeftAndColor leftNode color node =
        Node { node with Left = leftNode; Color = color }
    
    /// Creates a copy of the node with a different right child.
    let updateRight rightNode node = Node { node with Right = rightNode }
    
    let updateRightAndColor rightNode color node =
        Node { node with Right = rightNode; Color = color }

    let updateLeftAndRight leftNode rightNode node =
        Node { node with Left = leftNode; Right = rightNode }
    
    let updateLeftRightAndColor leftNode rightNode color node =
        Node { node with Left = leftNode; Right = rightNode; Color = color }
    
    let repaint color node =
        match node with
        | Node node -> { node with Color = color }
        | None -> invalidOp "bug: trying repaint a null node"
    
    /// Inserts an item into the subtree.
    let rec insert
        (log: LoggingFunc)
        (depth: int)
        (item: 'T)
        (grandparent: ParentRelation<'T>)
        (parent: Node<'T>)
        : InsertNodeResult<'T> =
        let msg = sprintf "visiting node %s" (parent |> Node |> nodeDesc)
        log depth msg
            
        if item < parent.Item then
            match parent.Left with
            | None ->
                match parent.Color with
                | Black ->
                    let leftNode = createLeaf item Red
                    parent |> updateLeft leftNode |> RepairParent
                | Red ->
                    match grandparent |> aunt |> color with
                    | Red ->
                        let leftNode = createLeaf item Red
                        parent
                        |> updateLeftAndColor leftNode Black
                        |> RepaintGrandparentAndAunt
                    | Black ->
                        match grandparent with
                        | LeftChildOf _ ->
                            let leftChild = createLeaf item Red
                            parent |> updateLeft leftChild |> RightRotate
                        | RightChildOf _ ->
                            log depth "left rotate 1"
                            create item Red None (Node parent) |> LeftRotate
                        | NoParent -> invalidOp "todo: NoParent"
            | Node leftNode ->
                log depth "moving left..."
                match leftNode
                      |> insert log (depth+1) item (leftChildOf parent) with
                | RepairParent leftNode' ->
                    match parent.Color with
                    | Black ->
                        parent |> updateLeft leftNode' |> RepairParent
                    | Red ->
                        match grandparent |> aunt |> color with
                        | Red ->
                            parent
                            |> updateLeftAndColor leftNode' Black
                            |> RepaintAunt
                        | Black ->
                            match grandparent with
                            | LeftChildOf _ -> invalidOp "todo: LeftChildOf"
                            | RightChildOf _ ->
                                log depth "left rotate 2"
                                parent |> updateLeft leftNode' |> LeftRotate
                            | NoParent -> invalidOp "todo: NoParent"
//                            parent |> updateLeft (Node leftNode') |> RightRotate
                | LeftRotate (Node leftNode') ->
                    log depth
                        (sprintf "left rotate on left node:\n%A" (leftNode' |> Node |> treeToDot ))
                    let parentRotatedToLeft: Tree<'T> =
                        parent
                        |> updateRightAndColor leftNode'.Left Red
                    let leftNodeRotatedToTop =
                        leftNode'
                        |> updateLeftAndColor parentRotatedToLeft Black
                    leftNodeRotatedToTop |> RepairParent
                | RightRotate (Node leftNode') ->
                    let parentRotatedToRight: Tree<'T> =
                        parent
                        |> updateLeftAndColor leftNode.Right Red
                    let leftNodeRotatedToTop =
                        leftNode'
                        |> updateRightAndColor parentRotatedToRight Black
                    leftNodeRotatedToTop |> RepairParent
                | RepaintAunt leftNode' ->
                    let repaintedAunt = parent.Right |> repaint Black |> Node
                    parent
                    |> updateLeftAndRight leftNode' repaintedAunt
                    |> RepairParent                    
                | RepaintGrandparentAndAunt leftNode' ->
                    let repaintedAunt = parent.Right |> repaint Black |> Node
                    parent
                    |> updateLeftRightAndColor leftNode' repaintedAunt Red
                    |> RepairParent
                | _ -> invalidOp "bug: this should never happen"
        else
            match parent.Right with
            | None ->
                match parent.Color with
                | Black ->
                    let rightNode = createLeaf item Red
                    parent |> updateRight rightNode |> RepairParent
                | Red ->
                    match grandparent |> aunt |> color with
                    | Red ->
                        let rightNode = createLeaf item Red
                        parent
                        |> updateRightAndColor rightNode Black
                        |> RepaintGrandparentAndAunt
                    | Black ->
                        match grandparent with
                        | LeftChildOf _ ->
                            create item Red (Node parent) None |> RightRotate
                        | RightChildOf _ ->
                            log depth "left rotate 3"
                            let rightChild = createLeaf item Red
                            parent |> updateRight rightChild |> LeftRotate
                        | NoParent -> invalidOp "todo: NoParent"
            | Node rightNode ->
                log depth "moving right..."
                match rightNode
                      |> insert log (depth+1) item (rightChildOf parent) with
                | RepairParent rightNode' ->
                    match parent.Color with
                    | Black -> 
                        parent |> updateRight rightNode' |> RepairParent
                    | Red ->
                        match grandparent |> aunt |> color with
                        | Red ->
                            parent
                            |> updateRightAndColor rightNode' Black
                            |> RepaintAunt
                        | Black ->
                            log depth "left rotate 4"                            
                            parent |> updateRight rightNode' |> LeftRotate
                | LeftRotate (Node rightNode') ->
                    let parentRotatedToLeft: Tree<'T> =
                        parent
                        |> updateRightAndColor rightNode'.Left Red
                    let rightNodeRotatedToTop =
                        rightNode'
                        |> updateLeftAndColor parentRotatedToLeft Black
                    rightNodeRotatedToTop |> RepairParent
                | RightRotate (Node rightNode') ->
                    let parentRotatedToRight: Tree<'T> =
                        parent
                        |> updateLeftAndColor rightNode'.Left Red
                    let rightNodeRotatedToTop =
                        rightNode'
                        |> updateRightAndColor parentRotatedToRight Black
                    rightNodeRotatedToTop |> RepairParent
                | RepaintAunt rightNode' ->
                    let repaintedAunt = parent.Left |> repaint Black |> Node
                    parent
                    |> updateLeftAndRight repaintedAunt rightNode'
                    |> RepairParent                    
                | RepaintGrandparentAndAunt rightNode' ->
                    let repaintedAunt = parent.Left |> repaint Black |> Node
                    parent
                    |> updateLeftRightAndColor
                           repaintedAunt rightNode' Red
                    |> RepairParent
                | _ -> invalidOp "bug: this should never happen"

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
            let node' = node |> updateLeft newLeftChild
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
            create successorNodeItem Black node.Left newRightChild

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
                node |> updateLeft left'
        else
            match node.Right with
            | None -> None
            | Node rightNode ->
                let right' = rightNode |> remove item
                node |> updateRight right'
    
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
                    node |> updateLeft newLeftNode |> Found
                | NotFound -> NotFound
        else
            match node.Right with
            | None -> NotFound
            | Node rightNode ->
                match tryRemove item rightNode with
                | Found newRightNode ->
                    node |> updateRight newRightNode |> Found
                | NotFound -> NotFound


/// Inserts an item into the tree and returns a new version of the tree.
let insert (log: LoggingFunc) item (tree: Tree<'T>) =
    log 0 (sprintf "Insert %A into tree: \n%A" item (tree |> treeToDot))
    
    match tree with
    | None -> Node.createLeaf item Black
    | Node rootNode ->
        match rootNode |> Node.insert log 0 item Node.NoParent with
        | Node.RepairParent (Node rootNode') ->
            match rootNode'.Color with
            | Black -> rootNode' |> Node
            | Red -> rootNode' |> Node |> Node.repaint Black |> Node
        | Node.LeftRotate rootNode' -> invalidOp "todo"
        | Node.RightRotate rootNode' -> invalidOp "todo"
        | Node.RepaintAunt rootNode' -> invalidOp "todo"
        | Node.RepaintGrandparentAndAunt rootNode' -> invalidOp "todo"
        | _ -> invalidOp "bug: this should never happen"
   
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

//
//let insert2 (log: LoggingFunc) item (tree: Tree<'T>) =
//    match tree with
//    | None -> Node.createLeaf item Black |> Node
//    | Node tree ->
//        let child = Node.createLeaf item Red
//        tree |> Node.updateLeft child
//        