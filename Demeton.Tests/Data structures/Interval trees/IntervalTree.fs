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

type Tree<'TItem, 'TIntervalValue> = AvlTree.Tree<Node<'TItem, 'TIntervalValue>>

let insert lowValue highValue interval tree =
    tree |> AvlTree.insert (treeFuncs lowValue highValue) interval 

let intervals lowValue highValue = AvlTree.items (treeFuncs lowValue highValue)

let remove lowValue highValue interval tree =
    tree |> AvlTree.remove (treeFuncs lowValue highValue) interval

let intervalsOverlap low1 high1 low2 high2 =
    (low2 >= low1 && low2 <= high1)
    || (high2 >= low1 && high2 <= high1)
    || (low2 < low1 && high2 > high1)

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
