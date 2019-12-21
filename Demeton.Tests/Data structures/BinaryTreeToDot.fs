module DataStructures.BinaryTreeToDot

open Text

/// Outputs node ID (and any other attributes) in DOT language.
let nodeToDot
    isNode (nodeAttributes: 'Tree -> string) 
    (node: 'Tree) (nodeCounter: int) output =
    if isNode node then
        let nodeId = sprintf "%d" nodeCounter
        output
        |> appendFormat "{0} [{1}]" [| nodeId; nodeAttributes node |]
        |> newLine |> ignore        
        nodeId
    else
        let nodeId = sprintf "null%d" nodeCounter
        output
        |> appendFormat "{0} [shape=point]" [| nodeId |]
        |> newLine |> ignore
        nodeId

/// Outputs the subtree in DOT language. 
let rec subtreeToDot
    isNode
    (nodeAttributes: 'Tree -> string) 
    (leftChild: 'Tree -> 'Tree)
    (rightChild: 'Tree -> 'Tree)
    (node: 'Tree) nodeId (nodeCounter: int) output: int =
  
    if isNode node then
        let nodeCounterBeforeLeft = nodeCounter + 1
        
        let left = leftChild node
        let leftNodeId =
            nodeToDot isNode nodeAttributes left nodeCounterBeforeLeft output

        output
        |> appendFormat "{0} -> {1}" [| nodeId; leftNodeId |]
        |> newLine |> ignore

        let nodeCounterBeforeRight =
            if isNode left then
                output
                |> subtreeToDot
                       isNode nodeAttributes leftChild rightChild
                       left leftNodeId (nodeCounterBeforeLeft + 1)
            else nodeCounterBeforeLeft + 1

        let right = rightChild node
        let rightNodeId =
            nodeToDot isNode nodeAttributes right nodeCounterBeforeRight output
        output
        |> appendFormat "{0} -> {1}" [| nodeId; rightNodeId |]
        |> newLine |> ignore

        let nodeCounterAfterRight =
            if isNode right then
                output
                |> subtreeToDot
                       isNode nodeAttributes leftChild rightChild
                       right rightNodeId (nodeCounterBeforeRight + 1)
            else nodeCounterBeforeRight + 1

        nodeCounterAfterRight
    else
        nodeCounter


/// Serializes the tree into string using the DOT language.
let treeToDot
    isNode (nodeAttributes: 'Tree -> string) leftChild rightChild
    (tree: 'Tree) =
    if isNode tree then
        let builder =
            buildString()
            |> appendLine "digraph BST {"
        
        let nodeId = builder |> nodeToDot isNode nodeAttributes tree 0
        builder
        |> subtreeToDot isNode nodeAttributes leftChild rightChild tree nodeId 0
        |> ignore
        
        builder
        |> appendLine "}"
        |> toString
    else ""