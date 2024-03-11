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

    let rec floodNeighbourPixel
        (surfaceArea: int)
        coverage
        (point: Point)
        : int * Rect =
        match point with
        | x, y when
            x < 0
            || y < 0
            || x >= waterHeightsArray.Width
            || y >= waterHeightsArray.Height
            ->
            surfaceArea, coverage
        | x, y ->
            let pixelColor = waterHeightsArray.heightAtLocal (x, y)

            if pixelColor = 1s then
                waterHeightsArray.setHeightAtLocal (x, y) color

                let surfaceArea, coverage =
                    floodNeighbourPixel surfaceArea coverage (x - 1, y)

                let surfaceArea, coverage =
                    floodNeighbourPixel surfaceArea coverage (x + 1, y)

                let surfaceArea, coverage =
                    floodNeighbourPixel surfaceArea coverage (x, y - 1)

                let surfaceArea, coverage =
                    floodNeighbourPixel surfaceArea coverage (x, y + 1)

                surfaceArea + 1, coverage.Extend(x, y)
            else
                surfaceArea, coverage

    let mutable currentPoint = Some startingPoint

    let mutable waterBody = None

    while currentPoint.IsSome do
        let pixelColor = waterHeightsArray.heightAtLocal currentPoint.Value

        let nextPoint = findNextPoint currentPoint

        match pixelColor, currentPoint.Value with
        | 1s, point ->
            // todo 0: found water, start coloring it
            waterHeightsArray.setHeightAtLocal point color

            let pointX, pointY = point

            let surfaceArea = 1

            let coverage =
                { MinX = pointX
                  MinY = pointY
                  Width = 1
                  Height = 1 }

            let surfaceArea, coverage =
                floodNeighbourPixel surfaceArea coverage (pointX - 1, pointY)

            let surfaceArea, coverage =
                floodNeighbourPixel surfaceArea coverage (pointX + 1, pointY)

            let surfaceArea, coverage =
                floodNeighbourPixel surfaceArea coverage (pointX, pointY - 1)

            let surfaceArea, coverage =
                floodNeighbourPixel surfaceArea coverage (pointX, pointY + 1)

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

let colorWaterBodies (waterHeightsArray: HeightsArray) : WaterBody seq =
    let rec colorWaterBodiesRec nextColor startingPoint waterBodies =
        match
            tryColorNextWaterBody nextColor startingPoint waterHeightsArray
        with
        | Some(waterBody, Some nextPoint) ->
            colorWaterBodiesRec
                (nextColor + 1s)
                nextPoint
                (Seq.append waterBodies (Seq.singleton waterBody))
        | Some(waterBody, None) ->
            Seq.append waterBodies (Seq.singleton waterBody)
        | None -> waterBodies

    colorWaterBodiesRec 2s (0, 0) Seq.empty
