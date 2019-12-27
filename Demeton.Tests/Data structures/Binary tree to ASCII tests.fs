module Tests.``Data structures``.``Binary tree to ASCII tests``

open DataStructures.BinaryTrees
open DataStructures.BinaryTrees.BinaryTree

open Xunit
open Swensen.Unquote
open Tests.``Data structures``.``Binary search tree testbed``

#if LOGGING
open TestLog

let logger = initLog "c:\\temp\\logs\\binary-tree.log"
let log message = logger.Information message     
#endif

let itemToString = function
    | UnbalancedBinarySearchTree.Node node -> sprintf "%A" node.Item
    | UnbalancedBinarySearchTree.None -> ""

let toAscii =
    treeToAscii
        UnbalancedBinarySearchTree.height
        UnbalancedBinarySearchTree.None
        UnbalancedBinarySearchTree.isNode
        itemToString
        UnbalancedBinarySearchTree.leftChild
        UnbalancedBinarySearchTree.rightChild
        
[<Fact>]
let ``Empty tree``() =
    let tree = UnbalancedBinarySearchTree.None

    let stringTree = toAscii tree
       
    test <@ stringTree = "[]" @>
    
[<Fact>]
let ``Single node tree``() =
    let tree =
        UnbalancedBinarySearchTree.None
        |> UnbalancedBinarySearchTree.insert { Value = 10; Tag = "A" }

    let stringTree = toAscii tree
       
    test <@ stringTree = @"
""10 A"" " @>
    
[<Fact>]
let ``Tree with height of 2``() =
    let tree =
        UnbalancedBinarySearchTree.None
        |> UnbalancedBinarySearchTree.insert { Value = 10; Tag = "A" }
        |> UnbalancedBinarySearchTree.insert { Value = 2; Tag = "B" }
        |> UnbalancedBinarySearchTree.insert { Value = 122; Tag = "C" }

    let stringTree = toAscii tree

    test <@ stringTree = @"
     ""10 A""     
    /     \     
 ""2 B""  ""122 C"" " @>

[<Fact>]
let ``Tree with height of 3``() =
    let tree =
        UnbalancedBinarySearchTree.None
        |> UnbalancedBinarySearchTree.insert { Value = 10; Tag = "A" }
        |> UnbalancedBinarySearchTree.insert { Value = 2; Tag = "B" }
        |> UnbalancedBinarySearchTree.insert { Value = 1; Tag = "D" }
        |> UnbalancedBinarySearchTree.insert { Value = 122; Tag = "E" }
        |> UnbalancedBinarySearchTree.insert { Value = 111; Tag = "F" }
        |> UnbalancedBinarySearchTree.insert { Value = 144; Tag = "G" }

    let stringTree = toAscii tree

    test <@ stringTree = @"
             ""10 A""             
        /             \         
     ""2 B""          ""122 E""     
    /               /     \     
 ""1 D""          ""111 F"" ""144 G"" " @>
        
[<Fact>]
let ``Test case 1``() =
    let tree =
        UnbalancedBinarySearchTree.None
        |> UnbalancedBinarySearchTree.insert { Value = 5; Tag = "A" }
        |> UnbalancedBinarySearchTree.insert { Value = 2; Tag = "B" }
        |> UnbalancedBinarySearchTree.insert { Value = 3; Tag = "C" }

    let stringTree = toAscii tree

    test <@ stringTree = @"
         ""5 A""          
      /                 
   ""2 B""                
       \                
      ""3 C""             " @>
