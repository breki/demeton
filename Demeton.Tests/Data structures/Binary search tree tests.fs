module Tests.``Data structures``.``Binary search tree tests``

open Xunit
open FsCheck

type private TreeOperation =
    | Insert of int
    | Remove of int
    | TryRemoveNonExisting of int 

[<Fact>]
let ``Binary search tree property test``() =
    let genInsert = Arb.generate<int> |> Gen.map Insert
    
    invalidOp "todo"