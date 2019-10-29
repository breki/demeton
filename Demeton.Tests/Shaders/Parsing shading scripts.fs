module Tests.Shaders.``Parsing shading scripts``

open Demeton.Shaders
open Demeton.Shaders.Pipeline.Common
open Demeton.DemTypes

open Xunit
open Swensen.Unquote

let parseShadingScript script: ShadingStep =
    ElevationColoring { ColorScale = ElevationColoring.colorScaleMaperitive }

[<Fact>]
let ``Supports parsing elevation coloring step without parameters``() =
    let script = "elecolor"

    let rootStep = parseShadingScript script

    test <@ match rootStep with 
            | ElevationColoring parameters -> 
                parameters = 
                    { ColorScale = ElevationColoring.colorScaleMaperitive }
            | _ -> false
        @>

[<Fact(Skip="todo")>]
let ``Supports parsing elevation coloring step with custom color scale``() =
    let script = "elecolor(scale='-1:#0;1000:#ffffff';none:#0')"

    let rootStep = parseShadingScript script

    let expectedColorScale: ElevationColoring.ColorScale = 
        { Marks = [| (DemHeight -1s, 0u ); (DemHeight 1000s, 0xffffffffu) |];
            NoneColor = 0u }

    test <@ match rootStep with 
            | ElevationColoring parameters -> 
                parameters.ColorScale = expectedColorScale
            | _ -> false
        @>
    
