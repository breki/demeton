module Demeton.Tests.Commands_tests.DemWithWaterBodiesCommand.Downsampling_water_bodies


open Demeton.WaterBodies.Funcs
open Xunit
open Swensen.Unquote



[<Fact>]
let ``Downsampling without fraction`` () =
    let scene =
        @"
0000100
0010100
1011110
1111100
0011100
0011000
0001000
"

    let heightsArray = Tests.Dem.HeightArraysScenes.parseScene scene 0 0

    let factor = 1. / 3.
    let downsampled = heightsArray |> downsampleWaterBodiesHeightsArray factor

    let downsampledScene = Tests.Dem.HeightArraysScenes.renderScene downsampled

    test
        <@
            downsampledScene = @"
01
11
"
        @>

[<Fact>]
let ``Downsampling with fraction`` () =
    let scene =
        @"
0000100
0010100
1011110
1111100
0011100
0011000
0001000
"

    let heightsArray = Tests.Dem.HeightArraysScenes.parseScene scene 0 0

    let factor = 1. / 1.5
    let downsampled = heightsArray |> downsampleWaterBodiesHeightsArray factor

    let downsampledScene = Tests.Dem.HeightArraysScenes.renderScene downsampled

    test
        <@
            downsampledScene = @"
0000
0111
1110
0110
"
        @>
