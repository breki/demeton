module Tests.WorldCover.RasterSimplification

open Demeton.DemTypes
open FsUnit
open Xunit
open Swensen.Unquote

let mapRasterValues mapFunc (heightsArray: HeightsArray) =
    let mappedCells = heightsArray.Cells |> Array.map mapFunc

    HeightsArray(
        heightsArray.MinX,
        heightsArray.MinY,
        heightsArray.Width,
        heightsArray.Height,
        HeightsArrayDirectImport mappedCells
    )


let convertWorldCoverRasterToWaterMonochrome
    (heightsArray: HeightsArray)
    : HeightsArray =
    let waterToMonochrome value =
        match value with
        // 80 represents water
        | 80s -> 1s
        | _ -> 0s

    heightsArray |> mapRasterValues waterToMonochrome


let sumCells9 x y (heightsArray: HeightsArray) =
    let rasterWidth = heightsArray.Width
    let rasterWidth2 = rasterWidth * 2

    let startingIndex =
        (y - 1 - heightsArray.MinY) * rasterWidth + x - 1 - heightsArray.MinX

    let rowCellsIndexes =
        [| 0
           1
           2
           rasterWidth
           rasterWidth + 1
           rasterWidth + 2
           rasterWidth2
           rasterWidth2 + 1
           rasterWidth2 + 2 |]

    let sum =
        rowCellsIndexes
        |> Array.sumBy (fun i -> heightsArray.Cells[startingIndex + i])

    sum, heightsArray.Cells[startingIndex + rasterWidth + 1]

// todo 2: introduce the loop into the function itself
let simplifyRaster (heightsArray: HeightsArray) : HeightsArray * int =
    let simplifiedHeightsArray =
        HeightsArray(
            heightsArray.MinX,
            heightsArray.MinY,
            heightsArray.Width,
            heightsArray.Height,
            HeightsArrayDirectImport(Array.copy heightsArray.Cells)
        )

    let processRasterLine y =
        let changedPixels =
            // todo 2: more optimized code that does not call heightAt(), it just
            // directly accesses the array
            seq { for x in heightsArray.MinX + 1 .. heightsArray.MaxX - 1 -> x }
            |> Seq.fold
                (fun acc x ->
                    let sum, pixelValue = heightsArray |> sumCells9 x y

                    if pixelValue = 1s then
                        if sum <= 4s then
                            simplifiedHeightsArray.setHeightAt (x, y) 0s
                            acc + 1
                        else
                            acc
                    elif sum >= 7s then
                        simplifiedHeightsArray.setHeightAt (x, y) 1s
                        acc + 1
                    else
                        acc)
                0

        changedPixels

    let totalChangedPixels =
        seq { for y in heightsArray.MinY + 1 .. heightsArray.MaxY - 1 -> y }
        |> Seq.toArray
        |> Array.Parallel.map processRasterLine
        |> Array.sum


    simplifiedHeightsArray, totalChangedPixels

// todo 3: write more tests
[<Fact>]
let icebreaker () =
    let heightsArray =
        HeightsArray(0, 0, 3, 3, HeightsArrayInitializer1D(fun _ -> 0s))

    heightsArray.setHeightAt (1, 1) 1s

    let simplified, changedPixels = heightsArray |> simplifyRaster

    test <@ simplified.heightAt (1, 1) = 0s @>
    test <@ changedPixels = 1 @>

[<Fact>]
let icebreaker2 () =
    let heightsArray =
        HeightsArray(0, 0, 3, 3, HeightsArrayInitializer1D(fun _ -> 1s))

    heightsArray.setHeightAt (1, 1) 0s

    let simplified, changedPixels = heightsArray |> simplifyRaster

    test <@ simplified.heightAt (1, 1) = 1s @>
    test <@ changedPixels = 1 @>
