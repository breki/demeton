/// Implementation of a persistent AVL tree.
///
/// Useful links:
/// - https://en.wikipedia.org/wiki/AVL_tree
/// - https://en.wikibooks.org/wiki/F_Sharp_Programming/Advanced_Data_Structures#AVL_Trees
/// - https://two-wrongs.com/purely-functional-avl-trees-in-common-lisp
[<RequireQualifiedAccess>]
module DataStructures.BinaryTrees.AvlTree

open DataStructures.BinaryTrees.BinaryTree
open System.Collections.Generic

/// The root of the binary search tree.
type Tree<'TNode> =
    | Node of 'TNode
    | None

type TreeFuncs<'TNode, 'TItem, 'TKey when 'TKey:comparison> = {
    NodeItem: 'TNode -> 'TItem
    ItemKey: 'TItem -> 'TKey
    Left: 'TNode -> Tree<'TNode>
    Right: 'TNode -> Tree<'TNode>
    Height: Tree<'TNode> -> int
    CreateNode: 'TItem -> Tree<'TNode> -> Tree<'TNode> -> Tree<'TNode>
}

let balanceFactor
    (treeFuncs: TreeFuncs<'TNode, 'TItem, 'TKey>) (tree: Tree<'TNode>) =
    match tree with
    | Node node -> 
        (node |> treeFuncs.Left |> treeFuncs.Height)
         - (node |> treeFuncs.Right |> treeFuncs.Height)
    | None -> 0

let isVeryLeftHeavy balanceFactor = balanceFactor >= 2
let isLeftHeavy balanceFactor = balanceFactor > 0
let isRightHeavy balanceFactor = balanceFactor < 0
let isVeryRightHeavy balanceFactor = balanceFactor <= -2

/// Returns true if the provided tree position represents a node or false if it
/// is None.
let isNode = function
    | Node _ -> true
    | None -> false

/// Serializes the tree into string using the DOT language.
let treeToDot
    (treeFuncs: TreeFuncs<'TNode, 'TItem, 'TKey>) (tree: Tree<'TNode>) =
    let nodeAttributes = function
        | Node node ->
            sprintf "label=%A" (treeFuncs.NodeItem node)
        | None -> ""
       
    let left treeFuncs tree =
        match tree with
        | Node node -> node |> treeFuncs.Left
        | _ -> invalidOp "the tree is not a node"
       
    let right treeFuncs tree =
        match tree with
        | Node node -> node |> treeFuncs.Right
        | _ -> invalidOp "the tree is not a node"
       
    tree
    |> treeToDot
           isNode nodeAttributes (left treeFuncs) (right treeFuncs)

let private itemToString (treeFuncs: TreeFuncs<'TNode, 'TItem, 'TKey>) =
    function
    | Node node ->
        sprintf "%A (%d)"
            (treeFuncs.NodeItem node)
            (node |> Node |> balanceFactor treeFuncs)
    | None -> ""

let treeToAscii treeFuncs tree =
    let itemToString = itemToString treeFuncs
    
    tree
    |> treeToAscii treeFuncs.Height None isNode itemToString

/// Contains internal implementation for inserting and removing of items from
/// the tree.
[<RequireQualifiedAccess>]
module private Node =
    /// Creates a copy of the node with a different left child.
    let updateLeft
        (treeFuncs: TreeFuncs<'TNode, 'TTItem, 'TKey>) leftNode node =
        treeFuncs.CreateNode
            (treeFuncs.NodeItem node)
            leftNode
            (treeFuncs.Right node)

    /// Creates a copy of the node with a different right child.
    let updateRight treeFuncs rightNode node =
        treeFuncs.CreateNode
            (treeFuncs.NodeItem node)
            (treeFuncs.Left node)
            rightNode
        
    /// Determines whether the tree contains the specified item. 
    let rec contains treeFuncs item node =
        let itemKey = item |> treeFuncs.ItemKey
        let nodeKey = node |> treeFuncs.NodeItem |> treeFuncs.ItemKey
        
        if item = (node |> treeFuncs.NodeItem) then true
        elif itemKey < nodeKey then
            match (treeFuncs.Left node) with
            | Node left -> left |> contains treeFuncs item
            | None -> false
        else
            match (treeFuncs.Right node) with
            | Node right -> right |> contains treeFuncs item
            | None -> false

    let rotateLeft treeFuncs (tree: Tree<'TNode>): Tree<'TNode> =
//        log (sprintf "rotateLeft - before: %s" (node |> treeToAscii))
        
        match tree with
        | Node node ->
            let right = treeFuncs.Right node
            
            match right with
            | Node rightNode ->
                let left = treeFuncs.Left node
                let item = treeFuncs.NodeItem node
                let rl = treeFuncs.Left rightNode
                let rr = treeFuncs.Right rightNode
                let ri = treeFuncs.NodeItem rightNode
                let left' = treeFuncs.CreateNode item left rl
                let rotated = treeFuncs.CreateNode ri left' rr
    //            log (sprintf "rotateLeft - after: %s" (rotated |> treeToAscii))
                rotated
            | _ -> tree
        | tree -> tree

    let rotateRight treeFuncs (tree: Tree<'TNode>): Tree<'TNode> =
//        log (sprintf "rotateRight - before: %s" (node |> treeToAscii))
        
        match tree with
        | Node node ->
            let left = treeFuncs.Left node
            
            match left with
            | Node leftNode ->
                let right = treeFuncs.Right node
                let item = treeFuncs.NodeItem node
                let ll = treeFuncs.Left leftNode
                let lr = treeFuncs.Right leftNode
                let li = treeFuncs.NodeItem leftNode
                
                let right' = treeFuncs.CreateNode item lr right
                let rotated = treeFuncs.CreateNode li ll right'
//            log (sprintf "rotateRight - after: %s" (rotated |> treeToAscii))
                rotated            
            | _ -> tree
        | tree -> tree
    
    let doubleRotateLeft treeFuncs (tree: Tree<'TNode>): Tree<'TNode> =
//        log (sprintf "doubleRotateLeft - before: %s" (node |> treeToAscii))
        
        match tree with
        | Node node ->
            let right': Tree<'TNode> =
                node |> treeFuncs.Right |> rotateRight treeFuncs
            let node' = node |> updateRight treeFuncs right'
            let rotated = node' |> rotateLeft treeFuncs
//            log (sprintf "doubleRotateLeft - after: %s" (rotated |> treeToAscii))
            rotated
        | node -> node
        
    let doubleRotateRight treeFuncs (tree: Tree<'TNode>): Tree<'TNode> =
//        log (sprintf "doubleRotateRight - before: %s" (node |> treeToAscii))
        
        match tree with
        | Node node ->
            let left' =
                node |> treeFuncs.Left |> rotateLeft treeFuncs
            let node' = node |> updateLeft treeFuncs left'
            let rotated = node' |> rotateRight treeFuncs
//            log (sprintf "doubleRotateRight - after: %s" (rotated |> treeToAscii))
            rotated
        | node -> node        
    
    let balance treeFuncs (tree: Tree<'TNode>): Tree<'TNode>  =
//        log (sprintf "balance %A" (node |> nodeItem))
        
        let nodeBalanceFactor = tree |> balanceFactor treeFuncs
        match tree with
        | Node n when nodeBalanceFactor |> isVeryLeftHeavy ->
            if n |> treeFuncs.Left |> balanceFactor treeFuncs
               |> isRightHeavy |> not then
                tree |> rotateRight treeFuncs
            else
                tree |> doubleRotateRight treeFuncs
        | Node n when nodeBalanceFactor |> isVeryRightHeavy ->
            if n |> treeFuncs.Right |> balanceFactor treeFuncs
               |> isLeftHeavy |> not then
                tree |> rotateLeft treeFuncs 
            else
                tree |> doubleRotateLeft treeFuncs
        | node -> node
    
    /// Looks for the successor (the leftest descendant) node and returns its
    /// item and a replacement node (recursively).
    let rec private removeSuccessor treeFuncs (node: 'TNode)
        : 'TItem * Tree<'TNode> =
//        log (sprintf "removeSuccessor %A" node.Item)

        match node |> treeFuncs.Left with
        // if we found the successor node, save its item and give its right
        // child to the successor parent
        | None -> (node |> treeFuncs.NodeItem, node |> treeFuncs.Right)
        // if there are still some nodes to the left...
        | Node left ->
            // find the successor and the new left child
            let (successorNodeItem, newLeftChild) =
                left |> removeSuccessor treeFuncs
            let node' =
                node
                |> updateLeft treeFuncs newLeftChild
                |> balance treeFuncs
            
            (successorNodeItem, node')
    
    /// Replaces (or, better, put, creates a new node of) the successor node of
    /// the right child.
    let private replaceWithSuccessor treeFuncs (node: 'TNode) =
//        log (sprintf "replaceWithSuccessor %A" node.Item)

        match node |> treeFuncs.Right with
        | None ->
            invalidOp
                "bug: this function should not be called on a node without the right child"
        | Node right -> 
            let (successorNodeItem, newRightChild) =
                right |> removeSuccessor treeFuncs
            treeFuncs.CreateNode
                successorNodeItem
                (node |> treeFuncs.Left)
                newRightChild
            |> balance treeFuncs

        
    /// The result type of the tryRemove function.
    type TryRemoveResult<'TNode> =
        /// The item was found in the specific subtree.
        | Found of Tree<'TNode>
        /// The item was not found.
        | NotFound
    
    /// Tries to remove an item from the subtree. If the item was not found,
    /// just returns NotFound result.
    let rec tryRemove
        (treeFuncs: TreeFuncs<'TNode, 'TItem, 'TKey>) item node
        : TryRemoveResult<'TNode> =

        let tryRemoveFromRightSubtree() =
            let rightTree = node |> treeFuncs.Right
            match rightTree with
            | None -> NotFound
            | Node rightNode ->
                match tryRemove treeFuncs item rightNode with
                | Found newRightNode ->
                    node
                    |> updateRight treeFuncs newRightNode
                    |> balance treeFuncs
                    |> Found
                | NotFound -> NotFound
                
        let useUpdatedLeftNodeAndReturnFound newLeftNode =
            node
            |> updateLeft treeFuncs newLeftNode
            |> balance treeFuncs
            |> Found
            
//        log (sprintf "tryRemove item %A from %A" item (node |> Node |> treeToAscii))

        let itemKey = item |> treeFuncs.ItemKey
        let nodeKey = node |> treeFuncs.NodeItem |> treeFuncs.ItemKey
        let leftTree = node |> treeFuncs.Left
        let rightTree = node |> treeFuncs.Right

        // if the item was found...
        if item = (node |> treeFuncs.NodeItem) then
            match leftTree, rightTree with
            | None, None -> Found None
            | Node left, None -> Node left |> Found
            | None, Node right -> Node right |> Found
            | Node _, Node _ ->
                node
                |> replaceWithSuccessor treeFuncs
                |> balance treeFuncs
                |> Found
        // if the item is not the same, but it has the same key...
        elif itemKey = nodeKey then
            // try to find the item first in the left subtree
            let leftTryRemoveResult =
                match leftTree with
                | None -> NotFound
                | Node leftNode -> tryRemove treeFuncs item leftNode
                
            match leftTryRemoveResult with
            | NotFound ->
                // and if not found in the left, try the right subtree
                tryRemoveFromRightSubtree()
            | Found newLeftNode -> useUpdatedLeftNodeAndReturnFound newLeftNode
        elif itemKey < nodeKey then
            match leftTree with
            | None -> NotFound
            | Node leftNode ->
                match tryRemove treeFuncs item leftNode with
                | Found newLeftNode -> useUpdatedLeftNodeAndReturnFound newLeftNode
                | NotFound -> NotFound
        else tryRemoveFromRightSubtree()
    
    /// Removes an item from the subtree. If the item was not found,
    /// throws an exception.
    let rec remove
        (treeFuncs: TreeFuncs<'TNode, 'TItem, 'TKey>)
        (item: 'TItem) (node: 'TNode) =
        
        let removeFromRightSubtree () =
            let rightTree = node |> treeFuncs.Right
            match rightTree with
            | None -> None
            | Node rightNode ->
                let right' = rightNode |> remove treeFuncs item
                node |> updateRight treeFuncs right' |> balance treeFuncs
        
//        log (sprintf "remove item %A from %s" item (node |> Node |> treeToAscii))

        let itemKey = item |> treeFuncs.ItemKey
        let nodeKey = node |> treeFuncs.NodeItem |> treeFuncs.ItemKey
        
        let leftTree = node |> treeFuncs.Left
        let rightTree = node |> treeFuncs.Right
        
        // if the item was found...
        if item = (node |> treeFuncs.NodeItem) then
            match leftTree, rightTree with
            | None, None -> None
            | Node left, None -> Node left
            | None, Node right -> Node right
            | Node _, Node _ ->
                node
                |> replaceWithSuccessor treeFuncs
                |> balance treeFuncs
        // if the item is not the same, but it has the same key...
        elif itemKey = nodeKey then
            // try to find the item first in the left subtree
            let leftTryRemoveResult =
                match leftTree with
                | None -> NotFound
                | Node leftNode -> tryRemove treeFuncs item leftNode
            match leftTryRemoveResult with
            | NotFound ->
                // and if not found in the left, use right subtree
                removeFromRightSubtree()
            | Found newLeftNode ->
                node
                |> updateLeft treeFuncs newLeftNode
                |> balance treeFuncs                
        elif itemKey < nodeKey then
            match leftTree with
            | None -> None
            | Node leftNode ->
                let left' = leftNode |> remove treeFuncs item
                node |> updateLeft treeFuncs left' |> balance treeFuncs
        else
            removeFromRightSubtree()

/// Inserts an item into the tree and returns a new version of the tree.
let rec insert treeFuncs item (tree: Tree<'TNode>) =
//    log (sprintf "insert %A under %A" item (tree |> itemToString))

    match tree with
    | None -> treeFuncs.CreateNode item None None
    | Node node ->
        let itemKey = item |> treeFuncs.ItemKey
        let nodeKey = node |> treeFuncs.NodeItem |> treeFuncs.ItemKey

        let node' =
            if itemKey < nodeKey then
                let left' = 
                    insert treeFuncs item (node |> treeFuncs.Left)
                node |> Node.updateLeft treeFuncs left'
            else
                let right' =
                    insert treeFuncs item (node |> treeFuncs.Right)
                node |> Node.updateRight treeFuncs right'
        Node.balance treeFuncs node'
   
/// Removes an item from the tree and returns a new version of the tree.
/// If the item was not found, throws an exception.
let remove treeFuncs item tree =
//    log (sprintf "remove item %A" item)

    match tree with
    | None ->
        KeyNotFoundException "The item was not found in the tree."
        |> raise
    | Node rootNode -> rootNode |> Node.remove treeFuncs item

/// Tries to remove an item from the tree. If the item was found and removed,
/// returns a new version of the tree. If the item was not found, returns the
/// original tree.
let tryRemove treeFuncs item tree =
//    log (sprintf "tryRemove item %A" item)

    match tree with
    | None -> None
    | Node rootNode ->
        rootNode |> Node.tryRemove treeFuncs item
        |> function
        | Node.Found newTree -> newTree
        | Node.NotFound -> tree
        
let contains treeFuncs item tree =
    match tree with
    | None -> false
    | Node rootNode -> rootNode |> Node.contains treeFuncs item
        
/// Returns a sequence containing all of the items in the tree, sorted by
/// item keys. 
let rec items treeFuncs tree =
    match tree with
    | None -> seq []
    | Node node ->
        let leftItems = node |> treeFuncs.Left |> items treeFuncs
        let rightItems = node |> treeFuncs.Right |> items treeFuncs
        rightItems
        |> Seq.append [ node |> treeFuncs.NodeItem ]
        |> Seq.append leftItems
