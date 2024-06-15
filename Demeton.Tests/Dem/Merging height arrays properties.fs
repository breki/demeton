module Tests.Dem.``Merging height arrays properties``

open Demeton.Dem.Types
open Raster

open Xunit
open FsCheck

let ``Properties of merging height arrays``
    (
        arraysToMerge: HeightsArray list,
        newArrayBounds: Rect
    ) =

    let findCellHeightInArrays (x, y) : DemHeight =
        arraysToMerge
        |> List.tryPick (fun (ha: HeightsArray) ->
            if
                x >= ha.MinX && x <= ha.MaxX && y >= ha.MinY && y <= ha.MaxY
            then
                ha.heightAt (x, y) |> Some
            else
                None)
        |> function
            | Some height -> height
            | None -> DemHeightNone

    let merged = Demeton.Dem.Funcs.merge newArrayBounds arraysToMerge

    // property: final array is None if the arrays list is empty
    let emptyList =
        match arraysToMerge with
        | [] ->
            (merged |> Option.isNone)
            |> Prop.classify true "Empty heights arrays list"
            |> Prop.label
                "merged array is None if the heights arrays list is empty"
        | _ -> true |> Prop.classify true "Non-empty heights arrays list"

    // property: final array is None if its width or height are 0
    let mergedNone =
        match newArrayBounds.Width = 0 || newArrayBounds.Height = 0 with
        | true ->
            (merged |> Option.isNone)
            |> Prop.classify true "Merging width or height = 0"
            |> Prop.label "merged array is None if its width or height are 0"
        | false -> true |> Prop.classify true "Merging width and height > 0"

    // property: final array has the appropriate bounds
    let arrayBounds =
        match merged with
        | Some mergedArray ->
            (mergedArray.MinX = newArrayBounds.MinX
             && mergedArray.MinY = newArrayBounds.MinY
             && mergedArray.Width = newArrayBounds.Width
             && mergedArray.Height = newArrayBounds.Height)
            |> Prop.label "merged array has requested bounds"
            |@ sprintf
                "%d<>%d || %d<>%d || %d<>%d || %d<>%d"
                mergedArray.MinX
                newArrayBounds.MinX
                mergedArray.MinY
                newArrayBounds.MinY
                mergedArray.Width
                newArrayBounds.Width
                mergedArray.Height
                newArrayBounds.Height
        | None -> true |> Prop.classify true "merged array is None"

    // property: final array's cells have values of one of the merging arrays
    let cellsAreFromSources =
        match merged with
        | Some mergedArray ->
            let comparison =
                [| for y in mergedArray.MinY .. mergedArray.MaxY do
                       for x in mergedArray.MinX .. mergedArray.MaxX do
                           let mergedCellHeight = mergedArray.heightAt (x, y)

                           let mergingCellsHeight =
                               findCellHeightInArrays (x, y)

                           yield mergedCellHeight = mergingCellsHeight |]

            comparison
            |> Array.exists not
            |> not
            |> Prop.label
                "final array's cells have values of one of the merging arrays or None"

        | None -> true |> Prop.label "true"

    emptyList .&. mergedNone .&. arrayBounds .&. cellsAreFromSources

[<Fact>]
let ``Merging height arrays`` () =
    // create a set of arrays
    // define new array's bounds

    let genMinX = Gen.choose (-100, 100)
    let genMinY = Gen.choose (-100, 100)
    let genWidth = Gen.choose (1, 10)
    let genWidth0 = Gen.choose (0, 10)
    let genHeight = Gen.choose (1, 10)
    let genHeight0 = Gen.choose (0, 10)

    let genRect =
        Gen.map4
            (fun minX minY width height ->
                { MinX = minX
                  MinY = minY
                  Width = width
                  Height = height })
            genMinX
            genMinY
            genWidth0
            genHeight0

    let genDemHeight =
        Gen.choose (-100, 100)
        |> Gen.map DemHeight
        |> Gen.optionOf
        |> Gen.map (fun heightMaybe ->
            match heightMaybe with
            | None -> DemHeightNone
            | Some height -> height)

    let genHeightArray =
        Gen.map4
            (fun minX minY width height ->
                let heightValues =
                    genDemHeight |> Gen.sample 100 (width * height)

                HeightsArray(
                    minX,
                    minY,
                    width,
                    height,
                    HeightsArrayInitializer1D(fun i -> heightValues.[i])
                ))
            genMinX
            genMinY
            genWidth
            genHeight

    let genListOfArrays = genHeightArray |> Gen.listOf |> Gen.resize 5

    Gen.zip genListOfArrays genRect |> Arb.fromGen |> Prop.forAll
    <| ``Properties of merging height arrays``
    |> Check.QuickThrowOnFailure
//  |> replayPropertyCheck (590087975,296671099)
