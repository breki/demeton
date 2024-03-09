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

let simplifyRaster
    minPixelsChanged
    (heightsArray: HeightsArray)
    : HeightsArray =
    let heightsArrayCopy (ha: HeightsArray) =
        HeightsArray(
            ha.MinX,
            ha.MinY,
            ha.Width,
            ha.Height,
            HeightsArrayDirectImport(Array.copy ha.Cells)
        )

    let processRasterLine
        (originalHeightsArray: HeightsArray)
        (simplifiedHeightsArray: HeightsArray)
        y
        =
        let changedPixels =
            seq {
                for x in
                    originalHeightsArray.MinX + 1 .. originalHeightsArray.MaxX
                                                     - 1 -> x
            }
            |> Seq.fold
                (fun acc x ->
                    let sum, pixelValue = originalHeightsArray |> sumCells9 x y

                    if pixelValue = 1s then
                        if sum <= 5s then
                            simplifiedHeightsArray.setHeightAt (x, y) 0s
                            acc + 1
                        else
                            acc
                    elif sum >= 6s then
                        simplifiedHeightsArray.setHeightAt (x, y) 1s
                        acc + 1
                    else
                        acc)
                0

        changedPixels

    let rec simplifyUntilAlmostNoChange originalHeightsArray =
        let simplifiedHeightsArray = heightsArrayCopy originalHeightsArray

        let totalChangedPixels =
            seq {
                for y in
                    originalHeightsArray.MinY + 1 .. originalHeightsArray.MaxY
                                                     - 1 -> y
            }
            |> Seq.toArray
            |> Array.Parallel.map (
                processRasterLine originalHeightsArray simplifiedHeightsArray
            )
            |> Array.sum

        if totalChangedPixels >= minPixelsChanged then
            simplifyUntilAlmostNoChange simplifiedHeightsArray
        else
            simplifiedHeightsArray

    simplifyUntilAlmostNoChange heightsArray

[<Fact>]
let singleWaterPixelIsRemoved () =
    let heightsArray =
        HeightsArray(0, 0, 3, 3, HeightsArrayInitializer1D(fun _ -> 0s))

    heightsArray.setHeightAt (1, 1) 1s

    let simplified = heightsArray |> simplifyRaster 2

    test <@ simplified.heightAt (1, 1) = 0s @>

[<Fact>]
let singleNonWaterPixelTurnsToWater () =
    let heightsArray =
        HeightsArray(0, 0, 3, 3, HeightsArrayInitializer1D(fun _ -> 1s))

    heightsArray.setHeightAt (1, 1) 0s

    let simplified = heightsArray |> simplifyRaster 2

    test <@ simplified.heightAt (1, 1) = 1s @>
