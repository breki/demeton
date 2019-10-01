module ``ResultSeq tests``

open Xunit
open Swensen.Unquote
open TestHelp

[<Fact>]
let ``When funcs sequence is empty, return an empty OK result``() =
    let result: Result<int list, string list> = 
        ResultSeq.fold [] "some input"

    test <@ result |> isOkValue [] @>    

[<Fact>]
let ``When all result functions were successful, returns a list of successful outputs``() =
    let func1 _ = Ok 123
    let func2 _ = Ok 234
    let func3 _ = Ok 345

    let result = 
        ResultSeq.fold 
            [ func1; func2; func3; ] 
            "some input"

    test <@ result |> isOkValue [ 345; 234; 123 ] @>    

[<Fact>]
let ``Returns a list of all collected errors from functions``() =
    let func1 _ = Error "msg1"
    let func2 _ = Ok 234
    let func3 _ = Error "msg2"
    let func4 _ = Ok 234

    let result = 
        ResultSeq.fold 
            [ func1; func2; func3; func4 ] 
            "some input"

    test <@ result |> isErrorData [ "msg2"; "msg1" ] @>
