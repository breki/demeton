module Tests.Dem.HeightArraysScenes


open System
open Text

open Demeton.Dem.Types


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

        let anyDifferences = ref false

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
                        anyDifferences := true
                        actualHeight
                    else
                        DemHeight(Int16.MinValue))
            )

        // if there were no differences...
        if !anyDifferences |> not then
            // ... just return an empty string
            ""
        else
            // otherwise return the differences as a scene
            renderScene diffHeightsArray
