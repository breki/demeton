module Tests.WorldCover.Water_bodies_coloring_tests


open System
open Demeton.DemTypes
open Text
open Xunit
open Swensen.Unquote
open Tests.WorldCover.WaterBodiesColoring
open Tests.WorldCover.WaterBodiesOutlining


/// <summary>
/// Parse a scene represented as a multiline string into a HeightsArray.
/// </summary>
let parseScene (scene: string) (minX: int) (minY: int) : HeightsArray =
    // remove empty lines
    let lines =
        scene.Split(
            [| Environment.NewLine |],
            StringSplitOptions.RemoveEmptyEntries
            ||| StringSplitOptions.TrimEntries
        )

    let width = lines.[0].Length
    let height = lines.Length

    HeightsArray(
        minX,
        minY,
        width,
        height,
        HeightsArrayInitializer2D(fun (x, y) ->
            (lines[y - minY][x - minX] - '0') |> DemHeight)
    )


/// <summary>
/// Render a HeightsArray into a multiline string.
/// </summary>
let renderScene (heightsArray: HeightsArray) : string =
    let sb = buildString () |> newLine

    for y in 0 .. heightsArray.Height - 1 do
        for x in 0 .. heightsArray.Width - 1 do
            let cellChar =
                match heightsArray.heightAtLocal (x, y) with
                | Int16.MinValue -> " "
                | value -> ((value |> char) + '0' |> string)

            sb |> append cellChar |> ignore

        sb |> newLine |> ignore

    sb |> toString

/// <summary>
/// Compare two height arrays as scenes and return any differences.
/// </summary>
let compareScenes (actualScene: string) (expectedScene: string) : string =
    let actualHeightsArray = parseScene actualScene 0 0
    let expectedHeightsArray = parseScene expectedScene 0 0

    if actualHeightsArray.Width <> expectedHeightsArray.Width then
        $"Widths differ: expected: %d{expectedHeightsArray.Width}, "
        + $"actual: %d{actualHeightsArray.Width}"
    elif actualHeightsArray.Height <> expectedHeightsArray.Height then
        $"Heights differ: expected: %d{expectedHeightsArray.Height}, "
        + $"actual: %d{actualHeightsArray.Height}"
    else
        // Construct a new heights array that will contain just the cells
        // that differ between the two arrays. All other cells will have
        // value Int16.MinValue. Also, record the information if there were
        // any actual differences.

        let differences = ref false

        let diffHeightsArray =
            HeightsArray(
                0,
                0,
                actualHeightsArray.Width,
                actualHeightsArray.Height,
                HeightsArrayInitializer2D(fun (x, y) ->
                    let actualHeight = actualHeightsArray.heightAtLocal (x, y)

                    let expectedHeight =
                        expectedHeightsArray.heightAtLocal (x, y)

                    if actualHeight <> expectedHeight then
                        differences := true
                        actualHeight
                    else
                        DemHeight(Int16.MinValue))
            )

        // if there were no differences...
        if !differences then
            // ... just return an empty string
            ""
        else
            // otherwise return the differences as a scene
            renderScene diffHeightsArray


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
00000200
00020200
02022320
02223200
00023200
00022000
00002000
00000000
" = ""
        @>
