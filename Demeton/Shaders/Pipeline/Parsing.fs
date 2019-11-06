module Demeton.Shaders.Pipeline.Parsing

open FParsec

type ParsedParameter = { Name: string; Value: string }

type ParsedStep = { 
    Name: string 
    Parameters: ParsedParameter list }

type ParsedScript = ParsedStep list

let isAny _ = true
let isAlphanumeric x = isAsciiLetter x || isDigit x
let isWhitespace x = isAnyOf [| ' '; '\t'; '\n'; '\r' |] x
let isNotQuote x = x <> '''
let unquotedParameterValueChars x = 
    x <> ')' && x <> ',' && isNotQuote x && not (isWhitespace x)
let pstringWS x = skipString x .>> spaces

let parseShadingScript script: Result<ParsedScript, TextParsers.ParsingError> =
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
        TextParsers.formatParsingFailure parserError