module Tests.``Data structures``.``Binary trees``.``Binary search tree testbed``

open System
open DataStructures.BinaryTrees

/// The item added to the binary search trees. Along with the Value (which
/// is used for comparison), the item also has a Tag which is used to identify
/// nodes for debugging purposes.
[<StructuredFormatDisplay("\"{Value} {Tag}\"")>]
type TestItem = {
    Value: int
    Tag: string
    }

    with
    
    override x.ToString() = sprintf "%d (%s)" x.Value x.Tag

type TestAvlNode = {
    Item: TestItem
    Left: AvlTree.Tree<TestAvlNode>
    Right: AvlTree.Tree<TestAvlNode>
    Height: int
}

/// The operation to perform on the tree.
type TreeOperation =
    /// Insert a new item into the tree.
    | Insert of int
    /// Remove an item from the tree. The float value from 0 to 1 is
    /// multiplied by the current size of the tree to calculate the index of
    /// the item to be removed. 
    | Remove of float
    /// Try to remove an item from the tree. There is no guarantee the item
    /// is present in the tree.
    | TryRemove of int
    | Contains of int 

/// The current state of the binary search tree property test.
type TreeTestCurrent<'Tree> = {
    /// The list which serves as a test oracle. The tree should contain the same
    /// items as this list (and in the same order).
    List: TestItem list
    /// The tree under the test.
    Tree: 'Tree
    /// Count of the tree operations performed so far.
    OperationsPerformed: int
}

/// Represents both the intermediate and final results of the binary search tree
/// property test. 
type TreeTestResult<'Tree> =
    Result<TreeTestCurrent<'Tree>, TreeTestCurrent<'Tree> * string>

type TreePropertyVerificationFunc<'Tree> =
    TreeTestCurrent<'Tree> -> TreeTestResult<'Tree>

