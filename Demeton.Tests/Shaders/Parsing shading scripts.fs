module Tests.Shaders.``Parsing shading scripts``

open Demeton.Shaders
open Demeton.Shaders.ShadingPipeline

open Xunit
open Swensen.Unquote

let parseShadingScript script: ShadingStep =
    ElevationColoring 

[<Fact>]
let ``Parsing elevation coloring step without parameters``() =
    let script = "elecolor"

    let rootStep = parseShadingScript script

    test <@ match rootStep with 
            | ElevationColoring -> true
            | _ -> false
        @>

