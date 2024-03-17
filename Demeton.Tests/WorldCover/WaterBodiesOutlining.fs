module Tests.WorldCover.WaterBodiesOutlining

open System
open Demeton.DemTypes
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
        // copy the box defined by waterBody.Coverage into a new
        // bracketed heights array

        let initializePixel (globalX: int, globalY: int) : DemHeight =
            if
                coloredWaterBodiesRaster.heightAt (globalX, globalY) = waterBody.Color
            then
                // a marker that the water body pixel was not yet visited
                Int16.MinValue
            else
                0s

        HeightsArray(
            waterBody.Coverage.MinX,
            waterBody.Coverage.MinY,
            waterBody.Coverage.Width,
            waterBody.Coverage.Height,
            HeightsArrayInitializer2D initializePixel
        )

    let anyNeighborPixelsOfDistance
        (distance: int16)
        (localX: int, localY: int)
        (waterBody: WaterBody)
        (waterBodyRaster: HeightsArray)
        : bool =
        let neighbors =
            [ (localX - 1, localY)
              (localX + 1, localY)
              (localX, localY - 1)
              (localX, localY + 1) ]

        neighbors
        |> Seq.exists (fun (localX, localY) ->
            localX >= 0
            && localX < waterBody.Coverage.Width
            && localY >= 0
            && localY < waterBody.Coverage.Height
            && waterBodyRaster.heightAtLocal (localX, localY) = distance)

    let outlineWaterBody
        (waterBody: WaterBody)
        distance
        (waterBodyRaster: HeightsArray)
        =
        for localY in 0 .. waterBody.Coverage.Height - 1 do
            for localX in 0 .. waterBody.Coverage.Width - 1 do
                let pixel = waterBodyRaster.heightAtLocal (localX, localY)

                if pixel = Int16.MinValue then
                    if
                        anyNeighborPixelsOfDistance
                            distance
                            (localX, localY)
                            waterBody
                            waterBodyRaster
                    then
                        waterBodyRaster.setHeightAtLocal
                            (localX, localY)
                            // todo 5: add Int16.MaxValue guard
                            (distance + 1s)
                    else
                        ()
                else
                    ()

        waterBodyRaster


    waterBodies
    |> Seq.map (fun waterBody ->
        let initialDistance = 0s

        { Raster =
            initializeWaterBodyRaster waterBody
            |> outlineWaterBody waterBody initialDistance
          DistanceFromOutlineStats = [] })
