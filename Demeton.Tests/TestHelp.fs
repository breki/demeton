module TestHelp

open System
open Swensen.Unquote

let isOk result =
    match result with
    | Ok _ -> true
    | Error _ -> false

let isOkValue expectedOkValue result =
    match result with
    | Ok value -> value = expectedOkValue
    | Error _ -> false

let resultValue result =
    match result with
    | Ok x -> x
    | Error msg -> 
        invalidOp (sprintf "The result indicates an error: '%s'." msg)

let isError (errorData: 'TError) (result: Result<'T, 'TError>) =
    match result with
    | Ok _ -> false
    | Error actualErrorData -> actualErrorData = errorData

let inline (=~=) (x: float) (y: float) = abs (x-y) <  1.E-10
    
let isApproxEqualTo (val2: float) (decimals: int) (val1: float) =
    test <@ 
            Math.Round(val2, decimals) = Math.Round(val1, decimals)
            @>