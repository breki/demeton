module Tests.``FParsec exploratory tests``

open FParsec

open System

open Xunit
open Swensen.Unquote

let parseSuccess parser str =
    match run parser str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> 
        invalidOp (sprintf "Expected success, but got '%s'" errorMsg)

let parseFailure parser str expectedError =
    match run parser str with
    | Success(_, _, _)   -> invalidOp "Expected failure"
    | Failure(errorMsg, _, _) -> 
        Assert.Contains(expectedError, errorMsg)
        true

[<Fact>]
let ``Parsing a float``() =
    test <@ parseSuccess pfloat "1.25" = 1.25 @>
    test <@ parseFailure pfloat "x" "Expecting: floating-point number" @>

[<Fact>]
let ``Parsing a prefix and a float``() =
    let parser = pchar '#' >>. pfloat

    test <@ parseSuccess parser "#1.25" = 1.25 @>
    test <@ parseFailure parser "1.25" "Error in Ln: 1 Col: 1" @>

[<Fact>]
let ``Using a tuple``() =
    let parser = tuple2 anyChar pfloat

    test <@ parseSuccess parser "#1.25" = ('#', 1.25) @>
    test <@ parseFailure parser "1.25" "Error in Ln: 1 Col: 2" @>

[<Fact>]
let ``Using a pipe``() =
    let parser = pipe2 anyChar pfloat (fun chr float -> (chr, float))

    test <@ parseSuccess parser "#1.25" = ('#', 1.25) @>
    test <@ parseFailure parser "1.25" "Error in Ln: 1 Col: 2" @>

[<Fact>]
let ``Parse hex char``() =
    let parser: Parser<char, unit> = hex

    test <@ parseSuccess parser "F" = 'F' @>
    test <@ parseFailure parser "X" "Expecting: hexadecimal digit" @>

[<Fact>]
let ``Using min max``() =
    let parser: Parser<string, unit> =
        manyMinMaxSatisfyL 2 4 isHex "hexadecimal digit"

    test <@ parseSuccess parser "F01" = "F01" @>
    test <@ parseFailure parser "X" "Expecting: hexadecimal digit" @>

let hexCharToInt c = 
    match c with
    | x when x >= '0' && x <= '9' -> (int c) - (int '0')
    | _ -> (int c) - (int 'A') + 10

let hexCharToUInt c = 
    match c with
    | x when x >= '0' && x <= '9' -> (uint32 c) - (uint32 '0')
    | _ -> (uint32 c) - (uint32 'A') + 10u

[<Fact>]
let ``Parse and convert hex char``() =
    let parser = hex |>> hexCharToInt

    test <@ parseSuccess parser "F" = 15 @>
    test <@ parseFailure parser "X" "Expecting: hexadecimal digit" @>

[<Fact>]
let ``Parse hex integer``() =
    let parser: Parser<int, unit> = 
        many1SatisfyL isHex "hexadecimal character"
        |>> fun digits ->
            digits
            |> Seq.toArray
            |> Array.rev 
            |> Array.fold 
                (fun sum digit -> sum * 16 + (digit |> hexCharToInt)) 0

    test <@ parseSuccess parser "FFFF" = 65535 @>
    test <@ parseFailure parser "X" "Expecting: hexadecimal character" @>

[<Fact>]
let ``Parse hex integer with prefix and length limit``() =
    let calcHex startingValue digits =
        digits
        |> Seq.toArray
        |> Array.fold 
            (fun sum digit -> 
                sum * 16u + (digit |> hexCharToUInt)) startingValue

    let parser: Parser<uint32, unit> =
        pstring "#" 
        >>. manyMinMaxSatisfyL 6 6 isHex "hex color value"
        .>>. opt (hex .>>. hex) .>> (eof <?> "hex color value")
        |>> fun (digits, additionalDigits) ->
            match additionalDigits with
            | Some (digit7, digit8) -> 
                calcHex 0u (digits + string digit7 + string digit8)
            | None -> calcHex 0xffu digits

    test <@ parseSuccess parser "#FF234567" = 4280501607u @>
    test <@ parseSuccess parser "#234567" = 4280501607u @>
    test <@ 
            parseFailure 
                parser "#2345" "Expecting: hex color value" @>
    test <@ 
            parseFailure 
                parser "234567" "Expecting: '#'" @>
    test <@ 
            parseFailure 
                parser "#123456789" "Expecting: hex color value" @>

