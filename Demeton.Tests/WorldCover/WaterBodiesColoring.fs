module Tests.WorldCover.WaterBodiesColoring

open System
open Demeton.DemTypes
open JetBrains.Profiler.Api
open Raster

type WaterBodyColor = int16

type SizePixels = int * int

type WaterBody =
    { Color: WaterBodyColor
      SurfaceArea: int
      Coverage: Rect }

let tryColorNextWaterBody
    (color: WaterBodyColor)
    (startingPoint: Point)
    (waterHeightsArray: HeightsArray)
    : (WaterBody * Point option) option =

    let findNextPoint point : Point option =
        match point with
        | Some(x, y) ->
            if x < waterHeightsArray.Width - 1 then Some(x + 1, y)
            elif y < waterHeightsArray.Height - 1 then Some(0, y + 1)
            else None
        | None -> None

    let mutable currentPoint = Some startingPoint

    let mutable waterBody = None

    let mutable cells = waterHeightsArray.Cells

    while currentPoint.IsSome do
        let pixelColor = waterHeightsArray.heightAtLocal currentPoint.Value

        let nextPoint = findNextPoint currentPoint

        match pixelColor, currentPoint.Value with
        | 1s, point ->
            // found water, start coloring it
            let mutable pointsToColor = List.singleton point
            let mutable surfaceArea = 0

            let mutable coverage = Rect.Empty
            let mutable stillMorePoints = true

            while pointsToColor |> List.isEmpty |> not do
                match pointsToColor with
                | [] -> ()
                | _ ->
                    let point = List.head pointsToColor
                    let pointX, pointY = point

                    pointsToColor <- List.tail pointsToColor

                    let pointIndex = pointY * 12000 + pointX

                    let pointColor = cells[pointIndex]

                    match pointColor with
                    | 1s ->
                        cells[pointIndex] <- color

                        surfaceArea <- surfaceArea + 1
                        coverage <- coverage.Extend(point)

                        // neighbour left
                        if pointX > 0 then
                            let neighborColor = cells[pointIndex - 1]

                            if neighborColor = 1s then
                                pointsToColor <-
                                    (pointX - 1, pointY) :: pointsToColor

                        // neighbour right
                        if pointX < (12000 - 1) then
                            let neighborColor = cells[pointIndex + 1]

                            if neighborColor = 1s then
                                pointsToColor <-
                                    (pointX + 1, pointY) :: pointsToColor

                        // neighbour up
                        if pointY > 0 then
                            let neighborColor = cells[pointIndex - 12000]

                            if neighborColor = 1s then
                                pointsToColor <-
                                    (pointX, pointY - 1) :: pointsToColor

                        // neighbour down
                        if pointY < (12000 - 1) then
                            let neighborColor = cells[pointIndex + 12000]

                            if neighborColor = 1s then
                                pointsToColor <-
                                    (pointX, pointY + 1) :: pointsToColor

                    | _ -> ()

            waterBody <-
                Some(
                    { Color = color
                      SurfaceArea = surfaceArea
                      Coverage = coverage },
                    nextPoint
                )

            currentPoint <- None
        | _ ->
            // it's either no water here, or another already colored water body,
            // move to the next pixel
            currentPoint <- nextPoint

    waterBody

let colorWaterBodies (waterHeightsArray: HeightsArray) : WaterBody list =
    MeasureProfiler.StartCollectingData()

    let rec colorWaterBodiesRec color startingPoint waterBodies =
        match tryColorNextWaterBody color startingPoint waterHeightsArray with
        | Some(waterBody, Some nextPoint) ->
            if color = Int16.MaxValue then
                failwith (
                    "Too many water bodies for int16 index... "
                    + "we need to start using int32"
                )

            let nextColor = color + 1s

            colorWaterBodiesRec nextColor nextPoint (waterBody :: waterBodies)
        | Some(waterBody, None) -> (waterBody :: waterBodies)
        | None -> waterBodies

    let result = colorWaterBodiesRec 2s (0, 0) List.empty |> List.rev

    MeasureProfiler.StopCollectingData()
    MeasureProfiler.SaveData()

    result
