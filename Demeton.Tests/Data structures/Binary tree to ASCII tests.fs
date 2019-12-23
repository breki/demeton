module Tests.``Data structures``.``Binary tree to ASCII tests``

open DataStructures

open Xunit
open Swensen.Unquote
open Tests.``Data structures``.``Binary search tree testbed``

type TreeLevels<'T when 'T:comparison> = 'T list []

type BinaryTreeBranch = Left | Right
type BinaryTreePath = BinaryTreeBranch list

type VisitedNode<'Tree> = ('Tree * int * BinaryTreePath)

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

let private emptyTreeLevels treeHeight =
    Array.create treeHeight (fun _ -> [])

let private addNodeToTreeLevels treeLevels (node, depth, path) =
    let indexFold (indexSoFar: int, depth: int) (branch: BinaryTreeBranch)
        : (int * int) =
        let branchValue =
            match branch with
            | Left -> 0
            | Right -> 1
        let newIndex = indexSoFar + branchValue * (1 <<< depth)
        (newIndex, depth + 1)
        
//    let nodePathToIndex (path: BinaryTreePath) =
//        match path with
//        | [] -> 0
//        | _ ->
//            path
//            |> Seq.rev
//            |> Seq.fold indexFold (0, 0)
    
    invalidOp "todo"

let toAscii
    height isNode itemToString leftChild rightChild
    tree =
        
    let nodeWidth = 4

    match isNode tree with
    | false -> "[]"
    | true ->
        let emptyTreeLevels = tree |> height |> emptyTreeLevels
        
        tree
        |> visitAllNodes isNode leftChild rightChild
        |> Seq.fold addNodeToTreeLevels emptyTreeLevels
        |> ignore
        
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
    let tree = UnbalancedBinarySearchTree.None

    let stringTree =
        toAscii height isNode itemToString leftChild rightChild tree
       
    test <@ stringTree = "[]" @>
    
[<Fact(Skip="todo")>]
let ``Single node tree``() =
    let tree =
        UnbalancedBinarySearchTree.None
        |> UnbalancedBinarySearchTree.insert { Value = 10; Tag = "A" }

    let stringTree =
        toAscii height isNode itemToString leftChild rightChild tree
       
    test <@ stringTree = @" 10 (A) " @>
    
[<Fact(Skip="todo")>]
let ``Tree with height of 2``() =
    let tree =
        UnbalancedBinarySearchTree.None
        |> UnbalancedBinarySearchTree.insert { Value = 10; Tag = "A" }
        |> UnbalancedBinarySearchTree.insert { Value = 2; Tag = "B" }
        |> UnbalancedBinarySearchTree.insert { Value = 122; Tag = "C" }

    let stringTree =
        toAscii height isNode itemToString leftChild rightChild tree
       
    test <@ stringTree = @"       10 (A)    
    |        |
   2 (A)  122 (A) " @>
