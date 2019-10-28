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
let parameterValueChars x = x <> ')'

let parseShadingScript script: ParsedScript =
    let pStepName: Parser<string, unit> = 
        many1Satisfy isAlphanumeric .>> spaces
    let pParameterName = many1Satisfy isAlphanumeric
    let pParameterValue = many1Satisfy parameterValueChars // not ')' or whitespace

    let pParameters = tuple4 (pstring "(") spaces (pstring ")") spaces
    let pStepNameAndParameters = pStepName .>> (opt pParameters)
    let pStepOperator = tuple2 (pstring "|+") spaces

    let pSteps = 
        sepBy pStepNameAndParameters pStepOperator
        |>> List.map (fun stepName -> { Name = stepName; Parameters = [] } )

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
    let script = "shader1 |+ shader2|+shader3"

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

[<Fact(Skip="todo")>]
let ``Can parse single unquoted parameter``() =
    let script = "shader1(par1=12)"

    test <@ parseShadingScript script = [ 
        { Name = "shader1"; Parameters = [ { Name = "par1"; Value = "12" } ] } ] 
        @>
    
