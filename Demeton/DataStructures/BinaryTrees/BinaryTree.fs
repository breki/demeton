/// Common data types and functions for binary trees.
module DataStructures.BinaryTrees.BinaryTree

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
