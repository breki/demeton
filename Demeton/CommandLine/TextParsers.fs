[<RequireQualifiedAccess>]
module CommandLine.TextParsers

open FParsec

let parseParameter(str: string) (parsingFunc: Parser<'u, unit>) =
    use charStream = new CharStream<unit>(str, 0, str.Length)

    let reply = parsingFunc charStream
    match reply.Status with
    | Ok -> Result.Ok reply.Result
    | _ -> Result.Error()
    
/// <summary>
/// Tries to parse a string argument as an 32-bit integer.
/// </summary>
let parseInt (str: string): Result<int, unit> =
    parseParameter str pint32

/// <summary>
/// Tries to parse a string argument as a float.
/// </summary>
let parseFloat (str: string): Result<float, unit> =
    parseParameter str pfloat

/// <summary>
/// Tries to parse a string argument as a comma-separated list of float values.
/// </summary>
let parseFloatsList (str: string): Result<float list, unit> =
    let parsingFunc = sepBy pfloat (pstring ",")
    parseParameter str parsingFunc

