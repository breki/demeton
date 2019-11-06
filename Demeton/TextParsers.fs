[<RequireQualifiedAccess>]
module TextParsers

open FParsec

type ParsingError = { Message: string; Location: int }

let formatParsingFailure (parserError: ParserError) =
    let location = int parserError.Position.Column - 1

    let allMessages = ErrorMessageList.ToSortedArray parserError.Messages

    let errorMessage =
        match allMessages with
        | [||] -> "Unknown error"
        | _ -> 
            let expectedTokens =
                allMessages
                |> Array.choose (fun err ->
                    match err with
                    | Expected x -> Some x
                    | ExpectedString x -> Some x
                    | _ -> None)

            match expectedTokens with
            | [||] ->
                let firstError = parserError.Messages.Head
                match firstError with
                | Unexpected x -> x
                | _ -> invalidOp "todo"
            | _ -> "Expected: " + (String.concat ", " expectedTokens)

    Result.Error { Message = errorMessage; Location = location }
    
let parseParameter(str: string) (parsingFunc: Parser<'u, unit>) =
    match run parsingFunc str with
    | Success (parsedValue, _, _) -> Result.Ok parsedValue
    | Failure (_, parserError, _) -> formatParsingFailure parserError
    
/// <summary>
/// Tries to parse a string argument as an 32-bit integer.
/// </summary>
let parseInt (str: string): Result<int, ParsingError> =
    parseParameter str pint32

/// <summary>
/// Tries to parse a string argument as a float.
/// </summary>
let parseFloat (str: string): Result<float, ParsingError> =
    parseParameter str pfloat

/// <summary>
/// Tries to parse a string argument as a comma-separated list of float values.
/// </summary>
let parseFloatsList (str: string): Result<float list, ParsingError> =
    let parsingFunc = sepBy pfloat (pstring ",")
    parseParameter str parsingFunc

