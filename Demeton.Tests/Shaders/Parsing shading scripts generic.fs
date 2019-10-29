module Tests.Shaders.``Parsing shading scripts generic``

open FParsec

open Xunit
open Swensen.Unquote

type ParsedParameter = { Name: string; Value: string }

type ParsedStep = { 
    Name: string 
    Parameters: ParsedParameter list }

type ParsedScript = ParsedStep list

type ParsingError = { Message: string; Location: int }

let isAny _ = true
let isAlphanumeric x = isAsciiLetter x || isDigit x
let isWhitespace x = isAnyOf [| ' '; '\t'; '\n'; '\r' |] x
let isNotQuote x = x <> '''
let unquotedParameterValueChars x = 
    x <> ')' && x <> ',' && isNotQuote x && not (isWhitespace x)
let pstringWS x = skipString x .>> spaces

let parseShadingScript script: Result<ParsedScript, ParsingError> =
    let pParameterName = 
        many1Satisfy isAlphanumeric .>> spaces <?> "step parameter name"
    let pParameterEndingQuote =
        (pstring "'") <?> "step parameter ending quote (')"
    let pParameterQuotedValue =
        (pstring "'") >>. manySatisfy isNotQuote .>> pParameterEndingQuote
    let pParameterUnqoutedValue = 
        many1Satisfy unquotedParameterValueChars <?> "step parameter value"
    let pParameterValue = 
        (pParameterUnqoutedValue <|> pParameterQuotedValue) .>> spaces
        <?> "step parameter value"
    let pParameterAssignmentOperator = 
        (pstringWS "=") <?> "step parameter assignment operator ('=')"
    let pParameter = 
        tuple3 pParameterName pParameterAssignmentOperator pParameterValue
        |>> fun (name, _, value) ->  { Name = name; Value = value }
    let pParameterSeparator = pstringWS "," <?> "step parameter separator (',')"
    let pParametersList = 
        sepBy pParameter pParameterSeparator <?> "step parameters list"
    let pParameters = 
        (between 
            (pstringWS "(") 
            (pstringWS ")" <?> "closing parenthesis (')')") 
            pParametersList)
        <?> "step parameters"
    let pStepName: Parser<string, unit> = 
        many1Satisfy isAlphanumeric .>> spaces <?> "step name"

    let pStepNameAndParameters = pStepName .>>. (opt pParameters)
    let pStepOperator = pstringWS "|+" <?> "step operator"

    let pSteps = 
        spaces 
        >>. (sepBy pStepNameAndParameters pStepOperator) 
        .>> (notFollowedByL 
            (many1Satisfy isAny) 
            "Unexpected character, shading step name was expected")
        //.>> (eof <?> "shading step name")
        |>> List.map (fun (stepName, parametersMaybe) -> 
            let parameters = 
                parametersMaybe |> Option.defaultValue []
            { Name = stepName; Parameters = parameters } )

    match run pSteps script with
    | Success (steps, _, _) -> Result.Ok steps
    | Failure (_, parserError, _) -> 
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

[<Fact>]
let ``Empty script returns empty steps list``() =
    test <@ parseShadingScript "   " = Result.Ok [ ] @>

[<Fact>]
let ``Can parse single step without parameters``() =
    let script = "shader1"

    test <@ parseShadingScript script = Result.Ok [ 
        { Name = "shader1"; Parameters = [] } ] @>

[<Fact>]
let ``Can parse multiple steps without parameters``() =
    let script = " shader1 |+ shader2|+shader3"

    test <@ parseShadingScript script = Result.Ok [ 
        { Name = "shader1"; Parameters = [] };
        { Name = "shader2"; Parameters = [] }; 
        { Name = "shader3"; Parameters = [] } ] 
        @>

[<Fact>]
let ``Can parse empty parameters``() =
    let script = "shader1( ) |+ shader2 ()"

    test <@ parseShadingScript script = Result.Ok [ 
        { Name = "shader1"; Parameters = [] }; 
        { Name = "shader2"; Parameters = [] } ] 
        @>

[<Fact>]
let ``Can parse single unquoted parameter``() =
    let script = "shader1(par1=12) |+ shader2(par2 = 23) |+ shader3 ( par3=34 )"

    test <@ parseShadingScript script = Result.Ok [ 
        { Name = "shader1"; Parameters = [ { Name = "par1"; Value = "12" } ] } 
        { Name = "shader2"; Parameters = [ { Name = "par2"; Value = "23" } ] } 
        { Name = "shader3"; Parameters = [ { Name = "par3"; Value = "34" } ] } 
        ] 
        @>

[<Fact>]
let ``Can parse single quoted parameter``() =
    let script = 
        "shader1(par1='1 2') |+ shader2(par2 = ' 23') |+ shader3 ( par3='34 ' )"

    test <@ parseShadingScript script = Result.Ok [ 
        { Name = "shader1"; Parameters = [ { Name = "par1"; Value = "1 2" } ] } 
        { Name = "shader2"; Parameters = [ { Name = "par2"; Value = " 23" } ] } 
        { Name = "shader3"; Parameters = [ { Name = "par3"; Value = "34 " } ] } 
        ] 
        @>

[<Fact>]
let ``Can parse multiple parameters``() =
    let script = "shader1(par1=12,par2='12', par3=dffgf ,par4=12.3)"

    test <@ parseShadingScript script = Result.Ok [ 
        { Name = "shader1"; 
            Parameters = [ 
                { Name = "par1"; Value = "12" } 
                { Name = "par2"; Value = "12" } 
                { Name = "par3"; Value = "dffgf" } 
                { Name = "par4"; Value = "12.3" } 
                ] } 
        ] 
        @>
   
[<Fact>]
let ``Unexpected character for step name``() =
    test <@ parseShadingScript " /" = 
        Result.Error 
            { Message = "Expected: step name"; Location = 1 } @>
   
[<Fact>]
let ``Missing operator``() =
    test <@ parseShadingScript "shader1  shader2" = 
        Result.Error 
            { Message = "Expected: step operator, step parameters"; 
                Location = 9 } @>
   
[<Fact>]
let ``Shader parameters not finished with closing parenthesis``() =
    test <@ parseShadingScript "shader1(par=1  shader2" = 
        Result.Error 
            { Message = "Expected: closing parenthesis (')'), step parameter separator (',')"; 
                Location = 15 } @>
   
[<Fact>]
let ``Missing shader parameter after comma separator``() =
    test <@ parseShadingScript "shader1(par=1, )" = 
        Result.Error 
            { Message = "Expected: step parameter name"; Location = 15 } @>
   
[<Fact>]
let ``Missing assignment operator after shader parameter``() =
    test <@ parseShadingScript "shader1(par )" = 
        Result.Error 
            { Message = "Expected: step parameter assignment operator ('=')"; 
                Location = 12 } @>
   
[<Fact>]
let ``Missing shader parameter value``() =
    test <@ parseShadingScript "shader1(par= )" = 
        Result.Error 
            { Message = "Expected: step parameter value"; 
                Location = 13 } @>
   
[<Fact>]
let ``Missing shader parameter ending quote``() =
    test <@ parseShadingScript "shader1(par='sdsd )" = 
        Result.Error 
            { Message = "Expected: step parameter ending quote (')"; 
                Location = 19 } @>
   
[<Fact>]
let ``Missing shader after the over operator``() =
    test <@ parseShadingScript "shader1(par='sdsd') |+ " = 
        Result.Error 
            { Message = "Expected: step name"; Location = 23 } @>
