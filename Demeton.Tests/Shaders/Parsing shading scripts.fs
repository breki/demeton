module Tests.Shaders.``Parsing shading scripts``

open Demeton.Shaders
open Demeton.Shaders.ShadingPipeline

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

[<Fact>]
let ``Supports parsing elevation coloring step with custom color scale``() =
    let script = "elecolor(scale='-1:#0;1000:#ffffff')"

    let rootStep = parseShadingScript script

    test <@ match rootStep with 
            | ElevationColoring parameters -> 
                // todo check the color scale
                true
            | _ -> false
        @>
    
