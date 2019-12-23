module Tests.``Data structures``.``Binary tree to ASCII tests``

open DataStructures
open Serilog

open System
open Xunit
open Swensen.Unquote
open Tests.``Data structures``.``Binary search tree testbed``

type BinaryTreeBranch = Left | Right
type BinaryTreePath = BinaryTreeBranch list

type VisitedNode<'Tree> = ('Tree * int * BinaryTreePath)

let logger =
    let outputTemplate =
        "{Timestamp:yy-MM-dd HH:mm:ss.fff} [{Level:u3}] {Message:lj}{NewLine}{Exception}"
    
    let logFileName = "c:\\temp\\logs\\binary-tree.log" 
    
    System.IO.File.Delete logFileName
    LoggerConfiguration().WriteTo
//        .RollingFile("c:\\temp\\logs\\binary-tree.log")
        .File(logFileName,
              Serilog.Events.LevelAlias.Minimum, outputTemplate)
        .CreateLogger()

let log message = logger.Information message 
    
let visitAllNodes<'Tree>
    isNode leftChild rightChild
    tree
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

type TreeLevels<'Tree> = 'Tree [] []

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
            |> visitAllNodes isNode leftChild rightChild
            |> Seq.fold addNodeToTreeLevels emptyTreeLevels

        // todo: remove this after the method works
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
                
        // find the widest node and use its width as the standard node width
        let nodeWidth =
            treeInLevels
            |> Seq.fold (fun maxWidth nodesInLevel ->
                let maxNodeWidthInLevel =
                    nodesInLevel 
                    |> Seq.fold (fun maxWidth node ->
                        let nodeString = node |> nodeToString
                        log nodeString
                        let nodeWidth = nodeString.Length
                        max nodeWidth maxWidth) 0
                max maxNodeWidthInLevel maxWidth) 0
    
        log (sprintf "max node width = %d" nodeWidth)
        
        let numberOfNodesOnBottom = treeInLevels |> Seq.last |> Seq.length
        
        // 1 is for the separator space between two adjacent nodes
        let treeAsciiWidth = numberOfNodesOnBottom * (1 + nodeWidth)
        
        let treeText =
            treeInLevels
            |> Seq.fold (fun treeText nodes ->
                let nodesInLevel = nodes.Length
                let spaceForEachNode = treeAsciiWidth / nodesInLevel
                
                let lineText = 
                    nodes
                    |> Seq.fold (fun line node ->
                        let nodeToString = node |> nodeToString
                        let paddingNeeded = (spaceForEachNode - nodeToString.Length) / 2 + 1
                        let padding = padding paddingNeeded
                        line + padding + nodeToString + padding) ""
                treeText + Environment.NewLine + lineText
    //            log (sprintf "space for level %d nodes: %d" level spaceForEachNode)
                ) ""

        log treeText
                
        invalidOp "todo"
//    let renderTreeRow level maxLeaves nodes output =
//        let nodesCount = 1 <<< level
//        let maxWidth = maxLeaves * nodeWidth
//        let indent = (maxWidth / (1 <<< level) - nodeWidth) / 2
//        
//        nodes
//        |> Seq.fo
//    
//    match tree with
//    | None -> "[empty tree]"
//    | Node node ->
//        let output = buildString()
//        
//        let height = height node
//        let maxLeaves = 2 <<< (height - 1)
//        
//        output |> renderTreeRow 0 maxLeaves [ node ] 

let height = UnbalancedBinarySearchTree.height

let emptyNode = UnbalancedBinarySearchTree.None

let isNode = function
    | UnbalancedBinarySearchTree.Node _ -> true
    | UnbalancedBinarySearchTree.None -> false
    
let itemToString = function
    | UnbalancedBinarySearchTree.Node node -> sprintf "%A" node.Item
    | UnbalancedBinarySearchTree.None -> ""

let leftChild = function
    | UnbalancedBinarySearchTree.Node ({Left = left}) -> left
    | _ -> UnbalancedBinarySearchTree.None

let rightChild = function
    | UnbalancedBinarySearchTree.Node ({Right = right}) -> right
    | _ -> UnbalancedBinarySearchTree.None

[<Fact>]
let ``Empty tree``() =
    let tree = emptyNode

    let stringTree =
        toAscii height emptyNode isNode itemToString leftChild rightChild tree
       
    test <@ stringTree = "[]" @>
    
//[<Fact(Skip="todo")>]
[<Fact>]
let ``Single node tree``() =
    let tree =
        UnbalancedBinarySearchTree.None
        |> UnbalancedBinarySearchTree.insert { Value = 10; Tag = "A" }

    let stringTree =
        toAscii height emptyNode isNode itemToString leftChild rightChild tree
       
    test <@ stringTree = @" 10 (A) " @>
    
//[<Fact(Skip="todo")>]
[<Fact>]
let ``Tree with height of 2``() =
    log "test"
    
    let tree =
        UnbalancedBinarySearchTree.None
        |> UnbalancedBinarySearchTree.insert { Value = 10; Tag = "A" }
        |> UnbalancedBinarySearchTree.insert { Value = 2; Tag = "B" }
        |> UnbalancedBinarySearchTree.insert { Value = 122; Tag = "C" }

    let stringTree =
        toAscii height emptyNode isNode itemToString leftChild rightChild tree

    test <@ stringTree = @"       10 (A)    
    |        |
   2 (A)  122 (A) " @>

//[<Fact(Skip="todo")>]
[<Fact>]
let ``Tree with height of 3``() =
    log "test"
    
    let tree =
        UnbalancedBinarySearchTree.None
        |> UnbalancedBinarySearchTree.insert { Value = 10; Tag = "A" }
        |> UnbalancedBinarySearchTree.insert { Value = 2; Tag = "B" }
        |> UnbalancedBinarySearchTree.insert { Value = 122; Tag = "C" }
        |> UnbalancedBinarySearchTree.insert { Value = 111; Tag = "E" }
        |> UnbalancedBinarySearchTree.insert { Value = 144; Tag = "D" }

    let stringTree =
        toAscii height emptyNode isNode itemToString leftChild rightChild tree

    test <@ stringTree = @"       10 (A)    
    |        |
   2 (A)  122 (A) " @>
