module Tests.WorldCover.RasterSimplification

open Demeton.Dem.Types
open Demeton.Dem.Funcs
open FsUnit
open Xunit
open Swensen.Unquote


// this function is currently not used in the production code
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
                        if sum <= 3s then
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
