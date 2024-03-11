module Tests.WorldCover.WaterBodiesColoring

open Demeton.DemTypes
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

    let addNeighbourPoint pointsToColor point =
        match point with
        | x, y when
            x < 0
            || y < 0
            || x >= waterHeightsArray.Width
            || y >= waterHeightsArray.Height
            ->
            pointsToColor
        | x, y ->
            let pixelColor = waterHeightsArray.heightAtLocal (x, y)

            if pixelColor = 1s then
                (x, y) :: pointsToColor
            else
                pointsToColor

    let mutable currentPoint = Some startingPoint

    let mutable waterBody = None

    while currentPoint.IsSome do
        let pixelColor = waterHeightsArray.heightAtLocal currentPoint.Value

        let nextPoint = findNextPoint currentPoint

        match pixelColor, currentPoint.Value with
        | 1s, point ->
            // found water, start coloring it
            let mutable pointsToColor = List.singleton point
            let mutable surfaceArea = 0

            let mutable coverage = Rect.Empty

            while pointsToColor |> List.length > 0 do
                match pointsToColor with
                | [] -> ()
                | _ ->
                    let point = List.head pointsToColor
                    pointsToColor <- List.tail pointsToColor

                    let pointColor = waterHeightsArray.heightAtLocal point

                    match pointColor with
                    | 1s ->
                        waterHeightsArray.setHeightAtLocal point color

                        surfaceArea <- surfaceArea + 1
                        coverage <- coverage.Extend(point)

                        let pointX, pointY = point

                        pointsToColor <-
                            addNeighbourPoint pointsToColor (pointX - 1, pointY)

                        pointsToColor <-
                            addNeighbourPoint pointsToColor (pointX + 1, pointY)

                        pointsToColor <-
                            addNeighbourPoint pointsToColor (pointX, pointY - 1)

                        pointsToColor <-
                            addNeighbourPoint pointsToColor (pointX, pointY + 1)

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
    let rec colorWaterBodiesRec nextColor startingPoint waterBodies =
        match
            tryColorNextWaterBody nextColor startingPoint waterHeightsArray
        with
        | Some(waterBody, Some nextPoint) ->
            colorWaterBodiesRec
                (nextColor + 1s)
                nextPoint
                (waterBody :: waterBodies)
        | Some(waterBody, None) -> (waterBody :: waterBodies)
        | None -> waterBodies

    colorWaterBodiesRec 2s (0, 0) List.empty |> List.rev
