module Tests.WorldCover.WaterBodiesOutlining

open System
open System.Diagnostics
open Demeton.DemTypes
open Raster
open Tests.WorldCover.WaterBodiesColoring

type WaterBodyOutline =
    { Raster: HeightsArray
      DistanceFromOutlineStats: int list }

// todo 10: document

let outlineWaterBodies
    (coloredWaterBodiesRaster: HeightsArray)
    (waterBodies: WaterBody seq)
    : WaterBodyOutline seq =

    let initializeWaterBodyRaster (waterBody: WaterBody) : HeightsArray =
        // Copy the box defined by waterBody.Coverage into a new
        // bracketed heights array. Note that we add one additional pixel around
        // the original waterBody.Coverage to make it easier to work with.

        // todo 0: debug this
        if waterBody.Color = 131s then
            let rect: Rect =
                { MinX = waterBody.Coverage.MinX - 1
                  MinY = waterBody.Coverage.MinY - 1
                  Width = waterBody.Coverage.Width + 2
                  Height = waterBody.Coverage.Height + 2 }

            let ext = coloredWaterBodiesRaster |> Demeton.Dem.extract rect
            Debugger.Break()

        let initializePixel (globalX: int, globalY: int) : DemHeight =
            if
                globalX >= coloredWaterBodiesRaster.MinX
                && globalX <= coloredWaterBodiesRaster.MaxX
                && globalY >= coloredWaterBodiesRaster.MinY
                && globalY <= coloredWaterBodiesRaster.MaxY
                && coloredWaterBodiesRaster.heightAt (globalX, globalY) = waterBody.Color
            then
                // a marker that the water body pixel was not yet visited
                Int16.MinValue
            else
                // this is not a water body pixel
                0s

        HeightsArray(
            waterBody.Coverage.MinX - 1,
            waterBody.Coverage.MinY - 1,
            waterBody.Coverage.Width + 2,
            waterBody.Coverage.Height + 2,
            HeightsArrayInitializer2D initializePixel
        )

    let anyNeighborPixelsOfDistance
        (distance: int16)
        (localX: int, localY: int)
        (waterBodyRaster: HeightsArray)
        : bool =
        let neighbors =
            [ (localX - 1, localY)
              (localX + 1, localY)
              (localX, localY - 1)
              (localX, localY + 1) ]

        neighbors
        |> Seq.exists (fun (x, y) ->
            x >= 0
            && x < waterBodyRaster.Width
            && y >= 0
            && y < waterBodyRaster.Height
            && waterBodyRaster.heightAtLocal (x, y) = distance)

    let outlineWaterBody
        (waterBody: WaterBody)
        distance
        (waterBodyRaster: HeightsArray)
        : HeightsArray * int =
        let mutable discoveredPixels = 0

        for localY in 0 .. waterBody.Coverage.Height - 1 do
            for localX in 0 .. waterBody.Coverage.Width - 1 do
                // "+ 1" is to account for the additional border pixels
                let pixel =
                    waterBodyRaster.heightAtLocal (localX + 1, localY + 1)

                if pixel = Int16.MinValue then
                    if
                        anyNeighborPixelsOfDistance
                            distance
                            (localX + 1, localY + 1)
                            waterBodyRaster
                    then
                        waterBodyRaster.setHeightAtLocal
                            (localX + 1, localY + 1)
                            // todo 5: add Int16.MaxValue guard
                            (distance + 1s)

                        discoveredPixels <- discoveredPixels + 1
                    else
                        ()
                else
                    ()

        waterBodyRaster, discoveredPixels


    waterBodies
    |> Seq.map (fun waterBody ->
        let mutable distance = 0s
        let mutable discoveredPixelsInTotal = 0

        let waterBodyRaster = initializeWaterBodyRaster waterBody

        // todo 0: debug this
        if waterBody.Color = 131s then
            let scene = HeightArraysScenes.renderScene waterBodyRaster
            ignore ()

        while discoveredPixelsInTotal < waterBody.SurfaceArea do
            let waterBodyRaster, discoveredPixels =
                waterBodyRaster |> outlineWaterBody waterBody distance

            if discoveredPixels > 0 then
                discoveredPixelsInTotal <-
                    discoveredPixelsInTotal + discoveredPixels

                distance <- distance + 1s
            else
                // todo 0: debug this
                raise (
                    InvalidOperationException(
                        "BUG: there should be at least one new pixel discovered."
                    )
                )

        { Raster = waterBodyRaster
          DistanceFromOutlineStats = [] })
