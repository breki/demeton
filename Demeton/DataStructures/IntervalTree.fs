/// Implementation of a persistent interval tree (using the persistent AVL tree)
/// as the underlying data structure).
[<RequireQualifiedAccess>]
module DataStructures.IntervalTree

open DataStructures.BinaryTrees

/// The interval-holding node type used in the underlying AVL tree.
/// 'TItem generic type is the type of the interval item each tree node holds.
/// 'TIntervalValue is the type of used by each interval's low and high values.
/// It needs to be sortable (comparable). 
type Node<'TItem, 'TIntervalValue> = {
    /// The interval item the node holds.
    Item: 'TItem
    /// The left subtree of the node.
    Left: AvlTree.Tree<Node<'TItem, 'TIntervalValue>>
    /// The right subtree of the node.
    Right: AvlTree.Tree<Node<'TItem, 'TIntervalValue>>
    /// The height of the subtree of which this node is the root.
    Height: int
    /// The maximum high value of the subtree of which this node is the root.
    MaxHigh: 'TIntervalValue
}

/// A function that returns the low value of a given interval item.
type LowValueFunc<'TItem, 'TIntervalValue> = 'TItem -> 'TIntervalValue
/// A function that returns the high value of a given interval item.
type HighValueFunc<'TItem, 'TIntervalValue> = 'TItem -> 'TIntervalValue

/// Returns the height of the given subtree.
let private height tree =
    match tree with
    | AvlTree.Node node -> node.Height
    | _ -> 0

/// Implementation of AVL tree interface specific for interval trees.
/// The implementation relies on two functions for returning the low and high
/// values for a given interval item.
let treeFuncs
    (lowValue: LowValueFunc<'TItem, 'TIntervalValue>) 
    (highValue: HighValueFunc<'TItem, 'TIntervalValue>) 
    : AvlTree.TreeFuncs<Node<'TItem, 'TIntervalValue>, 'TItem, 'TIntervalValue> = {
    NodeItem = fun node -> node.Item
    ItemKey = lowValue
    Left = fun node -> node.Left
    Right = fun node -> node.Right
    Height = height
    CreateNode = fun item left right ->
        let recalculateHeight left right =
            max (left |> height) (right |> height) + 1        
        
        let maxHigh
            (left: AvlTree.Tree<Node<'TItem, 'TIntervalValue>>)
            (right: AvlTree.Tree<Node<'TItem, 'TIntervalValue>>) =
            match left, right with
            | AvlTree.None, AvlTree.None -> item |> highValue
            | AvlTree.Node leftNode, AvlTree.None ->
                max leftNode.MaxHigh (item |> highValue)
            | AvlTree.None, AvlTree.Node rightNode ->
                max rightNode.MaxHigh (item |> highValue)
            | AvlTree.Node leftNode, AvlTree.Node rightNode ->
                max leftNode.MaxHigh (item |> highValue)
                |> max rightNode.MaxHigh
        
        { Item = item; Left = left; Right = right
          Height = recalculateHeight left right
          MaxHigh = maxHigh left right }
        |> AvlTree.Node
}

/// The root of the interval tree (or a subtree).
/// 'TItem generic type is the type of the interval item each tree node holds.
/// 'TIntervalValue is the type of used by each interval's low and high values.
/// It needs to be sortable (comparable). 
type Tree<'TItem, 'TIntervalValue> = AvlTree.Tree<Node<'TItem, 'TIntervalValue>>

/// Inserts a new interval into the interval tree.
let insert lowValue highValue interval tree =
    tree |> AvlTree.insert (treeFuncs lowValue highValue) interval 

/// Returns a sequence of all the intervals in the interval tree. 
let intervals lowValue highValue = AvlTree.items (treeFuncs lowValue highValue)

/// Removes an interval item from the tree and returns a new version of the
/// tree. If the interval item was not found, throws an exception.
let remove lowValue highValue interval tree =
    tree |> AvlTree.remove (treeFuncs lowValue highValue) interval

/// Indicates whether the two intervals overlap or not.
let intervalsOverlap low1 high1 low2 high2 =
    (low2 >= low1 && low2 <= high1)
    || (high2 >= low1 && high2 <= high1)
    || (low2 < low1 && high2 > high1)

/// Returns a sequence of interval items from the interval tree that overlap
/// the interval specified by its low and high values.
let rec findOverlapping lowValue highValue low high tree: 'Titem seq =
    seq {
        match tree with
        | AvlTree.None -> ignore()
        | AvlTree.Node node ->
            let item = node.Item
            let itemLow = item |> lowValue
            let itemHigh = item |> highValue
            
            if node.MaxHigh >= itemLow then
                if intervalsOverlap itemLow itemHigh low high then
                    yield item
                
                yield! findOverlapping lowValue highValue low high node.Left 
                yield! findOverlapping lowValue highValue low high node.Right 
    }
