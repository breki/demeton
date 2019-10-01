module Demeton.Commands.ParametersParsing

open FParsec

let parseFloatsList (str: string): Result<float list,string> =
    let parsingFunc = sepBy pfloat (pstring ",")

    use charStream = new CharStream<unit>(str, 0, str.Length)

    let reply = parsingFunc charStream
    match reply.Status with
    | Ok -> Result.Ok reply.Result
    | _ -> Result.Error "invalid"
