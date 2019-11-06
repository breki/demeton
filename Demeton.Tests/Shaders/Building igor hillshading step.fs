module Tests.``Shaders``.Building_igor_hillshading_step

open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.Pipeline.Parsing
open Demeton.Shaders.Pipeline.BuildingIgorHillshading
open Demeton.Geometry.Common

open Xunit
open Swensen.Unquote

[<Fact>]
let ``Can parse elevation coloring step without parameters``() =
    let parsedStep = { Name = "igor"; Parameters = [] } 

    let step = igorHillshadingStepBuilder parsedStep
    test
        <@ step =
            Ok (IgorHillshading
                 ({ SunAzimuth = degToRad -45.; ShadingColor = 0u })) 
        @>

[<Fact>]
let ``Can parse elevation coloring step with valid parameters``() =
    let parsedStep = 
        { Name = "igor"; 
        Parameters =
            [ { Name = "sunaz"; Value = "-90" };
                { Name = "shadcol"; Value = "#333333" } ] } 

    let step = igorHillshadingStepBuilder parsedStep
    test
        <@ step =
            Ok (IgorHillshading
                 ({ SunAzimuth = degToRad -90.; ShadingColor = 0x333333ffu })) 
        @>

[<Fact>]
let ``Reports an error when shading color is invalid``() =
    let parsedStep = 
        { Name = "igor"; 
        Parameters =
            [ { Name = "sunaz"; Value = "-90" };
                { Name = "shadcol"; Value = "wsdd" } ] } 

    let step = igorHillshadingStepBuilder parsedStep
    test <@ step = 
        Result.Error 
            ("Error in step 'igor': 'shadcol' parameter value error: " 
            + "invalid color value.") @>

[<Fact>]
let ``Reports an error when sun azimuth is invalid``() =
    let parsedStep = 
        { Name = "igor"; 
        Parameters =
            [ { Name = "sunaz"; Value = "xcd" };
                { Name = "shadcol"; Value = "#333333" } ] } 

    let step = igorHillshadingStepBuilder parsedStep
    test <@ step = 
        Result.Error 
            ("Error in step 'igor': 'sunaz' parameter value error: " 
            + "invalid degrees value.") @>
