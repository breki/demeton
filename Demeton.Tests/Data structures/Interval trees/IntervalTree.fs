[<RequireQualifiedAccess>]
module DataStructures.IntervalTree

open DataStructures.BinaryTrees

type Node<'TItem, 'TIntervalValue> = {
    Item: 'TItem
    Left: AvlTree.Tree<Node<'TItem, 'TIntervalValue>>
    Right: AvlTree.Tree<Node<'TItem, 'TIntervalValue>>
    Height: int
    MaxHigh: 'TIntervalValue
}

type LowValueFunc<'TItem, 'TIntervalValue> = 'TItem -> 'TIntervalValue
type HighValueFunc<'TItem, 'TIntervalValue> = 'TItem -> 'TIntervalValue

let private height tree =
    match tree with
    | AvlTree.Node node -> node.Height
    | _ -> 0

let treeFuncs (highValue: HighValueFunc<'TItem, 'TIntervalValue>) 
    : AvlTree.TreeFuncs<Node<'TItem, 'TIntervalValue>, 'TItem> = {
    NodeItem = fun node -> node.Item
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

type Tree<'TItem, 'TIntervalValue> = AvlTree.Tree<Node<'TItem, 'TIntervalValue>>

let insert highValue interval tree =
    tree |> AvlTree.insert (treeFuncs highValue) interval 

let intervals highValue = AvlTree.items (treeFuncs highValue)

let remove highValue interval tree =
    tree |> AvlTree.remove (treeFuncs highValue) interval

let findOverlapping highValue low high tree = []