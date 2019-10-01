module Demeton.Commands.ParametersParsing

open FParsec

let parseParameter(str: string) (parsingFunc: Parser<'u, unit>) =
    use charStream = new CharStream<unit>(str, 0, str.Length)

    let reply = parsingFunc charStream
    match reply.Status with
    | Ok -> Result.Ok reply.Result
    | _ -> Result.Error()
    

let parseFloat (str: string): Result<float, unit> =
    parseParameter str pfloat


let parseFloatsList (str: string): Result<float list, unit> =
    let parsingFunc = sepBy pfloat (pstring ",")
    parseParameter str parsingFunc
