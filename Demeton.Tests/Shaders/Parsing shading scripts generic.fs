module Tests.Shaders.``Parsing shading scripts generic``

open FParsec

open Xunit
open Swensen.Unquote

type ParsedParameter = { Name: string; Value: string }

type ParsedStep = { 
    Name: string 
    Parameters: ParsedParameter list }

type ParsedScript = ParsedStep list

let isAlphanumeric x = isAsciiLetter x || isDigit x
let isWhitespace x = isAnyOf [| ' '; '\t'; '\n'; '\r' |] x
let isNotQuote x = x <> '''
let parameterValueChars x = x <> ')' && isNotQuote x && not (isWhitespace x)
let pstringWS x = skipString x .>> spaces

let parseShadingScript script: ParsedScript =
    let pStepName: Parser<string, unit> = 
        many1Satisfy isAlphanumeric .>> spaces
    let pParameterName = many1Satisfy isAlphanumeric .>> spaces
    let pParameterQuotedValue =
        (pstring "'") >>. manySatisfy isNotQuote .>> (pstring "'")
    let pParameterUnqoutedValue = 
        many1Satisfy parameterValueChars <?> "parameter value"
    let pParameterValue = 
        (pParameterUnqoutedValue <|> pParameterQuotedValue) .>> spaces
    let pParameter = 
        tuple3 pParameterName (pstringWS "=") pParameterValue
        |>> fun (name, _, value) ->  { Name = name; Value = value }
    let pParameterSeparator = pstringWS ","
    let pParametersList = sepBy pParameter pParameterSeparator
    let pParameters = 
        between (pstringWS "(") (pstringWS ")") pParametersList 
    let pStepNameAndParameters = pStepName .>>. (opt pParameters)
    let pStepOperator = pstringWS "|+"

    let pSteps = 
        spaces >>. (sepBy pStepNameAndParameters pStepOperator)
        |>> List.map (fun (stepName, parametersMaybe) -> 
            let parameters = 
                parametersMaybe |> Option.defaultValue []
            { Name = stepName; Parameters = parameters } )

    match run pSteps script with
    | Success (steps, _, _) -> steps
    | Failure (errorMsg, _, _) -> invalidOp errorMsg

[<Fact>]
let ``Empty script returns empty steps list``() =
    test <@ parseShadingScript "   " = [ ] @>

[<Fact>]
let ``Can parse single step without parameters``() =
    let script = "shader1"

    test <@ parseShadingScript script = [ 
        { Name = "shader1"; Parameters = [] } ] @>

[<Fact>]
let ``Can parse multiple steps without parameters``() =
    let script = " shader1 |+ shader2|+shader3"

    test <@ parseShadingScript script = [ 
        { Name = "shader1"; Parameters = [] };
        { Name = "shader2"; Parameters = [] }; 
        { Name = "shader3"; Parameters = [] } ] 
        @>

[<Fact>]
let ``Can parse empty parameters``() =
    let script = "shader1( ) |+ shader2 ()"

    test <@ parseShadingScript script = [ 
        { Name = "shader1"; Parameters = [] }; 
        { Name = "shader2"; Parameters = [] } ] 
        @>

[<Fact>]
let ``Can parse single unquoted parameter``() =
    let script = "shader1(par1=12) |+ shader2(par2 = 23) |+ shader3 ( par3=34 )"

    test <@ parseShadingScript script = [ 
        { Name = "shader1"; Parameters = [ { Name = "par1"; Value = "12" } ] } 
        { Name = "shader2"; Parameters = [ { Name = "par2"; Value = "23" } ] } 
        { Name = "shader3"; Parameters = [ { Name = "par3"; Value = "34" } ] } 
        ] 
        @>

[<Fact>]
let ``Can parse single quoted parameter``() =
    let script = 
        "shader1(par1='1 2') |+ shader2(par2 = ' 23') |+ shader3 ( par3='34 ' )"

    test <@ parseShadingScript script = [ 
        { Name = "shader1"; Parameters = [ { Name = "par1"; Value = "1 2" } ] } 
        { Name = "shader2"; Parameters = [ { Name = "par2"; Value = " 23" } ] } 
        { Name = "shader3"; Parameters = [ { Name = "par3"; Value = "34 " } ] } 
        ] 
        @>
    
