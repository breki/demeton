[<RequireQualifiedAccess>]
module CommandLine.ValueParsers

open CommandLine.Common
open System.IO

let parseInt: OptionValueParser = fun text ->
    let intResult = TextParsers.parseInt text

    match intResult with
    | Error _ -> InvalidValue "it has to be an integer value"
    | Ok value -> OkValue value

let parsePositiveInt value =
    let intResult = TextParsers.parseInt value

    match intResult with
    | Error _ -> InvalidValue "it has to be an integer value larger than 0"
    | Ok value ->
        match value with
        | x when x < 1 -> 
            InvalidValue "it has to be an integer value larger than 0"
        | _ -> OkValue value

    
let parseDir: OptionValueParser = fun text -> OkValue text

let parseFloat minValue value =
    let invalidValue() =
        InvalidValue (sprintf "it has to be a numeric value >= %g" minValue)
    
    let floatResult = TextParsers.parseFloat value

    match floatResult with
    | Error _ -> invalidValue()
    | Ok value ->
        match value with
        | x when x < minValue -> invalidValue()
        | _ -> OkValue value

let parsePositiveFloat value =
    let floatResult = TextParsers.parseFloat value

    match floatResult with
    | Error _ -> InvalidValue "it has to be a positive numeric value"
    | Ok value ->
        match value with
        | x when x <= 0. -> 
            InvalidValue "it has to be a positive numeric value"
        | _ -> OkValue value


let parseFileName (value: string) =
    let checkedValue = Path.GetFileName(value)

    match checkedValue = value with
    | false -> InvalidValue "it has to consist of valid path characters"
    | true -> OkValue value
