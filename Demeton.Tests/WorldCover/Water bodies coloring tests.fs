module Tests.WorldCover.Water_bodies_coloring_tests

open Demeton.WorldCover.WaterBodiesColoring
open Demeton.WorldCover.WaterBodiesOutlining

open Tests.Dem.HeightArraysScenes

open Xunit
open Swensen.Unquote


[<Fact>]
let colorScene1 () =
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

    let someMinX = 10
    let someMinY = -100

    let heightsArray = parseScene scene someMinX someMinY

    let waterBodies = colorWaterBodies heightsArray

    let coloredWaterBodiesScene = renderScene heightsArray

    test
        <@
            coloredWaterBodiesScene = @"
0000200
0020200
2022220
2222200
0022200
0022000
0002000
"
        @>

    test <@ waterBodies.Length = 1 @>
    test <@ waterBodies.Head.Color = 2s @>
    test <@ waterBodies.Head.SurfaceArea = 19 @>

    test
        <@
            waterBodies.Head.Coverage = { MinX = someMinX
                                          MinY = someMinY
                                          Width = 6
                                          Height = 7 }
        @>

[<Fact>]
let colorScene2 () =
    let scene =
        @"
0000100
0010100
1011110
1111100
0011100
0011010
0001001
"

    let someMinX = 10
    let someMinY = -100

    let heightsArray = parseScene scene someMinX someMinY

    let waterBodies = colorWaterBodies heightsArray

    let coloredWaterBodiesScene = renderScene heightsArray

    test
        <@
            coloredWaterBodiesScene = @"
0000200
0020200
2022220
2222200
0022200
0022030
0002004
"
        @>

    test <@ waterBodies.Length = 3 @>
    test <@ waterBodies[1].Color = 3s @>
    test <@ waterBodies[1].SurfaceArea = 1 @>
    test <@ waterBodies[2].Color = 4s @>
    test <@ waterBodies[2].SurfaceArea = 1 @>

[<Fact>]
let colorScene3 () =
    let scene =
        @"
000
001
011
000
"

    let someMinX = 10
    let someMinY = -100

    let heightsArray = parseScene scene someMinX someMinY

    let waterBodies = colorWaterBodies heightsArray

    let coloredWaterBodiesScene = renderScene heightsArray

    test
        <@
            compareScenes
                coloredWaterBodiesScene
                @"
000
002
022
000
" = ""
        @>

    test <@ waterBodies.Length = 1 @>
    test <@ waterBodies.Head.Color = 2s @>
    test <@ waterBodies.Head.SurfaceArea = 3 @>

    test
        <@
            waterBodies.Head.Coverage = { MinX = someMinX + 1
                                          MinY = someMinY + 1
                                          Width = 2
                                          Height = 2 }
        @>

[<Fact>]
let outlineScene1 () =
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

    let someMinX = 10
    let someMinY = -100

    let heightsArray = parseScene scene someMinX someMinY

    let waterBodies = colorWaterBodies heightsArray

    let waterBodiesOutlines =
        outlineWaterBodies heightsArray waterBodies |> Seq.toArray

    test <@ waterBodiesOutlines.Length = 1 @>

    let outlinedWaterBodyScene = renderScene waterBodiesOutlines.[0].Raster

    test
        <@
            compareScenes
                outlinedWaterBodyScene
                @"
00000000
00000100
00010100
01011210
01122100
00012100
00011000
00001000
00000000
" = ""
        @>

[<Fact>]
let ``Water body is on the edge of raster`` () =
    let scene =
        @"
0011
0001
"

    let someMinX = 10
    let someMinY = -100

    let heightsArray = parseScene scene someMinX someMinY

    let waterBodies = colorWaterBodies heightsArray

    let waterBodiesOutlines =
        outlineWaterBodies heightsArray waterBodies |> Seq.toArray

    test <@ waterBodiesOutlines.Length = 1 @>

    let outlinedWaterBodyScene = renderScene waterBodiesOutlines.[0].Raster

    test
        <@
            compareScenes
                outlinedWaterBodyScene
                @"
0000
0110
0010
0000
" = ""
        @>
