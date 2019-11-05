module Tests.``Shaders``.Building_igor_hillshading_step

open Demeton.Shaders
open Demeton.Shaders.Pipeline.Building
open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.Pipeline.Parsing
open Png
open Demeton.Geometry.Common

open Xunit
open Swensen.Unquote

let igorHillshadingStepBuilder: ShadingStepBuildingFunc = fun parsedStep ->
    invalidOp "todo"

[<Fact(Skip="todo")>]
let ``Can parse elevation coloring step without parameters``() =
    let parsedStep = { Name = "igor"; Parameters = [] } 

    let step = igorHillshadingStepBuilder parsedStep
    test
        <@ step =
            Ok
                (IgorHillshading
                    ({ SunAzimuth = degToRad -45.; ShadingColor = 0u })) 
        @>
