module TestHelp

open System

let fail errorMessage =
    raise (Xunit.Sdk.XunitException errorMessage)

let noCall _ = invalidOp "bug: should not be called"
let noCall2 _ _ = invalidOp "bug: should not be called"
let noCall3 _ _ _ = invalidOp "bug: should not be called"
let noCall4 _ _ _ _ = invalidOp "bug: should not be called"

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

let isError (result: Result<'T, 'TError>) =
    match result with
    | Ok _ -> false
    | Error _ -> true

let isErrorData (errorData: 'TError) (result: Result<'T, 'TError>) =
    match result with
    | Ok _ -> false
    | Error actualErrorData -> actualErrorData = errorData

let inline (=~=) (x: float) (y: float) = abs (x-y) <  1.E-10
    
type ApproxMeasure =
    Decimals of int
    | Percentage of float

let isApproxEqualTo 
    (controlValue: float) (measure: ApproxMeasure) (actualValue: float) =
    match measure with
    | Decimals decimals -> 
        Math.Round(controlValue, decimals) 
            = Math.Round(actualValue, decimals)
    | Percentage percentage ->
        let percentageValue = controlValue * percentage / 100.
        Math.Abs(actualValue - controlValue) < percentageValue
