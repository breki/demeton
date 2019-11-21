module Tests.Shaders.``Parsing shading scripts``

open Demeton.Shaders
open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.Pipeline.BuildingElevationColoring
open Demeton.DemTypes
open Demeton.Commands
open CommandLine.Common

open Xunit
open Swensen.Unquote
open TestHelp

let private parseShadingScript script: ShadingStep =
    match ShadeCommand.parseShadingScriptOption script with
    | OkValue parsedValue -> parsedValue :?> ShadingStep
    | InvalidValue errorMessage -> 
        fail (sprintf "parsing of shading script failed: %s" errorMessage)

let private isElevationColoring step =
    match step with
    | ElevationColoring _ -> true
    | _ -> false

let private isIgorHillshading step =
    match step with
    | IgorHillshading _ -> true
    | _ -> false
    

[<Fact>]
let ``Supports parsing elevation coloring step without parameters``() =
    let script = StepNameElevationColoring

    let rootStep = parseShadingScript script

    test <@ match rootStep with 
            | ElevationColoring parameters -> 
                parameters = 
                    { ColorScale = ElevationColoring.colorScaleMaperitive }
            | _ -> false
        @>

[<Fact>]
let ``Supports parsing elevation coloring step with custom color scale``() =
    let script = "elecolor(scale='-1:#00000000;1000:#ffffff;none:#00000000')"

    let rootStep = parseShadingScript script

    let expectedColorScale: ElevationColoring.ColorScale = 
        { Marks = [| (DemHeight -1s, 0u ); (DemHeight 1000s, 0xffffffffu) |];
            NoneColor = 0u }

    test <@ match rootStep with 
            | ElevationColoring parameters -> 
                parameters.ColorScale = expectedColorScale
            | _ -> false
        @>

[<Fact>]
let ``Supports parsing compositing steps``() =
    let script = "elecolor |+ igor"

    let rootStep = parseShadingScript script

    test <@ match rootStep with
            | Compositing (step1, step2, _) ->
                step1 |> isElevationColoring && step2 |> isIgorHillshading
            | _ -> false
        @>
    
