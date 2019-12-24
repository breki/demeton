/// Common data types and functions for binary trees.
module DataStructures.BinaryTrees.BinaryTree

open Text
open System

/// Indicates which branch (from the parent) node belongs to.
type BinaryTreeBranch = Left | Right

/// Returns the other (opposite) branch.
let otherBranch = function
    | Left -> Right
    | Right -> Left

/// A path from the root of the tree to a node, consisting of left or right
/// directions. 
type BinaryTreePath = BinaryTreeBranch list

/// Structure returned by allNodes function for each node in the tree.
/// Consists of the tree node, its level and the path to it.
type VisitedNode<'Tree> = ('Tree * int * BinaryTreePath)

/// Returns all of the nodes of a binary tree. The tree is traversed using
/// the provided functions:
/// - isNode for determining whether a current position is a node or empty,
/// - leftChild which returns the left child node for a given node,
/// - rightChild which returns the right child node for a given node.
let allNodes<'Tree> isNode leftChild rightChild tree
    : VisitedNode<'Tree> seq =
        
    let rec visitPrivate node level path: VisitedNode<'Tree> seq =
        seq { 
            match isNode node with
            | false -> ignore()
            | true ->
                yield (node, level, path)
                yield! visitPrivate
                    (node |> leftChild) (level + 1) (Left :: path)
                yield! visitPrivate
                    (node |> rightChild) (level + 1) (Right :: path)
        }
            
    visitPrivate tree 0 []

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
    

type private TreeLevels<'Tree> = 'Tree [] []

let private initTreeLevels (emptyNode: 'Tree) treeHeight: TreeLevels<'Tree> =
    [| 1 .. treeHeight |]
    |> Array.map (fun level ->
        let levelNodesCount = 1 <<< (level - 1)
        Array.init levelNodesCount (fun _ -> emptyNode))

let private addNodeToTreeLevels (treeLevels: TreeLevels<'Tree>) (node, depth, path) =
    let indexFold (indexSoFar, depth) (branch: BinaryTreeBranch)
        : (int * int) =
        let branchValue =
            match branch with
            | Left -> 0
            | Right -> 1
        let newIndex = indexSoFar + branchValue * (1 <<< depth)
        (newIndex, depth + 1)
        
    let nodePathToIndex (path: BinaryTreePath) =
        match path with
        | [] -> 0
        | _ ->
            let (index, _) =
                path
                |> Seq.rev
                |> Seq.fold indexFold (0, 0)
            index
    
    let nodeIndexInLevel = path |> nodePathToIndex
    treeLevels.[depth].[nodeIndexInLevel] <- node
    treeLevels

let private padding count = String.init count (fun _ -> " ")

let toAscii
    (height: 'Tree -> int) emptyNode isNode nodeToString leftChild rightChild
    (tree: 'Tree) =
        
    match isNode tree with
    | false -> "[]"
    | true ->
        let emptyTreeLevels = tree |> height |> (initTreeLevels emptyNode)
        
        let treeInLevels =
            tree
            |> allNodes isNode leftChild rightChild
            |> Seq.fold addNodeToTreeLevels emptyTreeLevels

#if LOGGING
        treeInLevels
        |> Seq.iter (fun levelNodes ->
            let levelText =
                levelNodes
                |> Seq.map (fun node ->
                    match isNode node with
                    | true -> nodeToString node
                    | false -> "None")
                |> String.concat ", "
            log levelText)
#endif
                
        // find the widest node and use its width as the standard node width
        let maxNodeWidth =
            treeInLevels
            |> Seq.fold (fun maxWidth nodesInLevel ->
                let maxNodeWidthInLevel =
                    nodesInLevel 
                    |> Seq.fold (fun maxWidth node ->
                        let nodeString: string = node |> nodeToString
                        let nodeWidth = nodeString.Length
                        max nodeWidth maxWidth) 0
                max maxNodeWidthInLevel maxWidth) 0
    
#if LOGGING
        log (sprintf "max node width = %d" maxNodeWidth)
#endif
        
        let numberOfNodesOnBottom = treeInLevels |> Seq.last |> Seq.length
        
        // 1 is for the separator space between two adjacent nodes
        let treeAsciiWidth = numberOfNodesOnBottom * (1 + maxNodeWidth)
        
#if LOGGING
        log (sprintf "treeAsciiWidth=%d" treeAsciiWidth)
#endif
        
        let treeText =
            treeInLevels
            |> Seq.fold (fun treeText nodes ->
                let nodesInLevel = nodes.Length
                let spaceForEachNode = treeAsciiWidth / nodesInLevel
                let isFirstLevel = nodesInLevel = 1
                
                let connectorsLine =
                    if isFirstLevel then ""
                    else
                        let (connectorsLineText, _) =
                            nodes
                            |> Seq.fold (fun (line, branch) node ->
                                let connector =
                                    match (isNode node, branch) with
                                    | false, _ -> ""
                                    | _, Left -> "/"
                                    | _, Right -> "\\"
                                    
                                let connectorWidth = connector.Length
                                let oddPrefixPadding = 
                                    if connectorWidth % 2 = 0 then ""
                                    else " "
                                let paddingNeeded =
                                    (spaceForEachNode - connectorWidth) / 2
                                let padding = padding paddingNeeded
                                (line + padding + oddPrefixPadding + connector + padding, otherBranch branch)
                                ) ("", Left)
                        connectorsLineText + Environment.NewLine
                
                let nodesLineText = 
                    nodes
                    |> Seq.fold (fun line node ->
                        let nodeToString = node |> nodeToString
                        let nodeWidth = nodeToString.Length
                        let oddPrefixPadding = 
                            if nodeWidth % 2 = 0 then ""
                            else " "
                        let paddingNeeded = (spaceForEachNode - nodeWidth) / 2
                        let padding = padding paddingNeeded
//                        log (sprintf "'%s' paddingNeeded=%d" nodeToString paddingNeeded)
                        line + padding + oddPrefixPadding + nodeToString + padding) ""
                    
                treeText + Environment.NewLine
                    + connectorsLine
                    + nodesLineText
                ) ""

#if LOGGING
        log treeText
#endif
                
        treeText
