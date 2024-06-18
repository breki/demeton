module Demeton.WorldCover.WaterBodiesColoring

open System
open Demeton.Dem.Types
// open JetBrains.Profiler.Api
open Raster

type WaterBodyColor = int16

type SizePixels = int * int

type WaterBody =
    { Color: WaterBodyColor
      SurfaceArea: int
      Coverage: Rect }

let tryColorNextWaterBody
    (color: WaterBodyColor)
    (startingPointLocal: Point)
    (waterHeightsArray: HeightsArray)
    : (WaterBody * Point option) option =

    let findNextPoint pointLocal : Point option =
        match pointLocal with
        | Some(localX, localY) ->
            if localX < waterHeightsArray.Width - 1 then
                Some(localX + 1, localY)
            elif localY < waterHeightsArray.Height - 1 then
                Some(0, localY + 1)
            else
                None
        | None -> None

    let mutable currentPointLocal = Some startingPointLocal

    let mutable waterBody = None

    let mutable cells = waterHeightsArray.Cells

    let heightsArrayWidth = waterHeightsArray.Width
    let heightsArrayHeight = waterHeightsArray.Height

    while currentPointLocal.IsSome do
        let pixelColor = waterHeightsArray.heightAtLocal currentPointLocal.Value

        let nextPointLocal = findNextPoint currentPointLocal

        match pixelColor, currentPointLocal.Value with
        | 1s, point ->
            // found water, start coloring it
            let mutable pointsToColor = List.singleton point
            let mutable surfaceArea = 0

            let mutable coverage = Rect.Empty

            while pointsToColor |> List.isEmpty |> not do
                match pointsToColor with
                | [] -> ()
                | _ ->
                    let pointLocal = List.head pointsToColor
                    let pointLocalX, pointLocalY = pointLocal

                    pointsToColor <- List.tail pointsToColor

                    let pointIndex =
                        pointLocalY * heightsArrayWidth + pointLocalX

                    let pointColor = cells[pointIndex]

                    match pointColor with
                    | 1s ->
                        cells[pointIndex] <- color

                        surfaceArea <- surfaceArea + 1

                        let pointGlobal =
                            (waterHeightsArray.MinX + pointLocalX,
                             waterHeightsArray.MinY + pointLocalY)

                        coverage <- coverage.Extend(pointGlobal)

                        // neighbour left
                        if pointLocalX > 0 then
                            let neighborColor = cells[pointIndex - 1]

                            if neighborColor = 1s then
                                pointsToColor <-
                                    (pointLocalX - 1, pointLocalY)
                                    :: pointsToColor

                        // neighbour right
                        if pointLocalX < (heightsArrayWidth - 1) then
                            let neighborColor = cells[pointIndex + 1]

                            if neighborColor = 1s then
                                pointsToColor <-
                                    (pointLocalX + 1, pointLocalY)
                                    :: pointsToColor

                        // neighbour up
                        if pointLocalY > 0 then
                            let neighborColor =
                                cells[pointIndex - heightsArrayWidth]

                            if neighborColor = 1s then
                                pointsToColor <-
                                    (pointLocalX, pointLocalY - 1)
                                    :: pointsToColor

                        // neighbour down
                        if pointLocalY < (heightsArrayHeight - 1) then
                            let neighborColor =
                                cells[pointIndex + heightsArrayWidth]

                            if neighborColor = 1s then
                                pointsToColor <-
                                    (pointLocalX, pointLocalY + 1)
                                    :: pointsToColor

                    | _ -> ()

            waterBody <-
                Some(
                    { Color = color
                      SurfaceArea = surfaceArea
                      Coverage = coverage },
                    nextPointLocal
                )

            currentPointLocal <- None
        | _ ->
            // it's either no water here, or another already colored water body,
            // move to the next pixel
            currentPointLocal <- nextPointLocal

    waterBody

let colorWaterBodies (waterHeightsArray: HeightsArray) : WaterBody list =
    // MeasureProfiler.StartCollectingData()
    Log.debug "Coloring water bodies..."

    let rec colorWaterBodiesRec color startingPoint waterBodies =
        match tryColorNextWaterBody color startingPoint waterHeightsArray with
        | Some(waterBody, Some nextPointLocal) ->
            if color = Int16.MaxValue then
                failwith (
                    "Too many water bodies for int16 index... "
                    + "we need to start using int32"
                )

            let nextColor = color + 1s

            colorWaterBodiesRec
                nextColor
                nextPointLocal
                (waterBody :: waterBodies)
        | Some(waterBody, None) -> (waterBody :: waterBodies)
        | None -> waterBodies

    let result = colorWaterBodiesRec 2s (0, 0) List.empty |> List.rev

    Log.debug "Detected %d water bodies" result.Length

    // MeasureProfiler.StopCollectingData()
    // MeasureProfiler.SaveData()

    result
