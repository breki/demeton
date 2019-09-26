module TestHelp

let isOk result =
    match result with
    | Ok _ -> true
    | Error _ -> false

let resultValue result =
    match result with
    | Ok x -> x
    | Error msg -> 
        invalidOp (sprintf "The result indicates an error: '%s'." msg)

let isError errorMessage result =
    match result with
    | Ok _ -> false
    | Error actualMessage -> actualMessage = errorMessage
