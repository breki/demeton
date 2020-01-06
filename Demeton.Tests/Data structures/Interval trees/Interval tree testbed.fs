module Tests.``Data structures``.``Interval trees``.``Interval tree testbed``

open DataStructures

[<StructuredFormatDisplay("\"{Low}-{High} {Tag}\"")>]
type TestInterval = {
    Low: int
    High: int
    Tag: string
}

/// The operation to perform on the tree.
type TreeOperation =
    /// Insert a new interval into the tree.
    | Insert of (int * int)
    /// Remove an interval from the tree. The float value from 0 to 1 is
    /// multiplied by the current size of the tree to calculate the index of
    /// the item to be removed. 
    | Remove of float
    | Overlapping of (int * int) 

/// The current state of the interval tree property test.
type TreeTestCurrent = {
    /// The list which serves as a test oracle. The tree should contain the same
    /// intervals as this list (and in the same order).
    List: TestInterval list
    /// The tree under the test.
    Tree: IntervalTree.Tree
    /// Count of the tree operations performed so far.
    OperationsPerformed: int
}

/// Represents both the intermediate and final results of the binary search tree
/// property test. 
type TreeTestResult =
    Result<TreeTestCurrent, TreeTestCurrent * string>

type TreePropertyVerificationFunc = TreeTestCurrent -> TreeTestResult

let insertIntoList interval list =
    match list |> Seq.tryFindIndex (fun x -> x.Low > interval.Low) with
    | Some index -> list |> ListEx.insertAt index interval
    | None -> list @ [ interval ]

let findOverlapping low high list =
    list
    |> List.filter (fun interval ->
        (interval.Low >= low && interval.Low <= high)
        || (interval.High >= low && interval.High <= high)
        || (interval.Low < low && interval.High > high))