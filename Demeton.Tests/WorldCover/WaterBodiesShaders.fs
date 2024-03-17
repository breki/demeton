module Tests.WorldCover.WaterBodiesShaders

open System
open System.Threading.Tasks

open Demeton.DemTypes
open Demeton.Geometry.Common
open Demeton.Projections.Common
open Demeton.Shaders.Types
open Demeton.Srtm.Funcs
open Png
open Raster
open WorldCoverRaster

open Tests.WorldCover.WaterBodiesColoring
open Tests.WorldCover.WaterBodiesOutlining



/// <summary>
/// Get the value of a cell in a heights array for a given tile pixel.
/// </summary>
let valueForTilePixel
    x
    y
    cellsPerDegree
    inverse
    (heightsArray: HeightsArray)
    : DemHeight option =
    let lonLatOption = inverse (float x) (float -y)

    match lonLatOption with
    | None -> None
    | Some(lonRad, latRad) ->
        let lonDeg = radToDeg lonRad
        let latDeg = radToDeg latRad

        let globalSrtmX =
            lonDeg |> longitudeToCellX cellsPerDegree |> Math.Round |> int

        let globalSrtmY =
            latDeg |> latitudeToCellY cellsPerDegree |> Math.Round |> int

        heightsArray.heightAt (globalSrtmX, globalSrtmY) |> Some


/// <summary>
/// Returns a raster shader that accepts a WorldCover-originated heights array
/// and renders water bodies in blue and everything else in transparent.
/// </summary>
let worldCoverWaterBodiesShader
    heightsArrayIndex
    (waterBodies: WaterBody list)
    : RasterShader =
    fun heightsArrays srtmLevel tileRect imageData forward inverse ->
        let cellsPerDegree = cellsPerDegree WorldCoverTileSize srtmLevel

        let tileWidth = tileRect.Width

        let waterColor =
            match (Rgba8Bit.tryParseColorHexValue "#49C8FF") with
            | Ok color -> color
            | Error _ -> failwith "Could not parse color"

        let ignoredWaterColor =
            match (Rgba8Bit.tryParseColorHexValue "#FF96D1") with
            | Ok color -> color
            | Error _ -> failwith "Could not parse color"

        let errorWaterColor =
            match (Rgba8Bit.tryParseColorHexValue "#FF4800") with
            | Ok color -> color
            | Error _ -> failwith "Could not parse color"

        let greenWaterColor = Rgba8Bit.rgbaColor 0uy 255uy 0uy 255uy
        let redWaterColor = Rgba8Bit.rgbaColor 255uy 0uy 0uy 255uy
        let blueWaterColor = Rgba8Bit.rgbaColor 0uy 0uy 255uy 255uy
        let yellowWaterColor = Rgba8Bit.rgbaColor 255uy 255uy 0uy 255uy
        let magentaWaterColor = Rgba8Bit.rgbaColor 255uy 0uy 255uy 255uy
        let cyanWaterColor = Rgba8Bit.rgbaColor 0uy 255uy 255uy 255uy

        let palette =
            [| greenWaterColor
               redWaterColor
               blueWaterColor
               yellowWaterColor
               magentaWaterColor
               cyanWaterColor |]

        let noWaterColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 0uy

        let processRasterLine y =
            for x in tileRect.MinX .. (tileRect.MaxX - 1) do
                let rasterValue =
                    heightsArrays[heightsArrayIndex]
                    |> valueForTilePixel x y cellsPerDegree inverse

                let pixelValue =
                    match rasterValue with
                    | None -> noWaterColor
                    | Some 0s -> noWaterColor
                    | Some 1s -> errorWaterColor
                    | Some waterBodyColor ->
                        let waterBody = waterBodies.[int waterBodyColor - 2]

                        let totalArea =
                            waterBody.Coverage.Width * waterBody.Coverage.Height

                        match
                            waterBody.SurfaceArea,
                            totalArea / waterBody.SurfaceArea
                        with
                        | surfaceArea, _ when surfaceArea < 1250 ->
                            ignoredWaterColor
                        | _, coverageRatio when coverageRatio >= 10 ->
                            ignoredWaterColor
                        | _ -> waterColor

                Rgba8Bit.setPixelAt
                    imageData
                    tileWidth
                    (x - tileRect.MinX)
                    (y - tileRect.MinY)
                    pixelValue

        Parallel.For(tileRect.MinY, tileRect.MaxY, processRasterLine) |> ignore

let worldCoverWaterBodiesOutlineShader
    (waterBodies: WaterBody list)
    (waterBodiesOutlines: WaterBodyOutline seq)
    : RasterShader =

    let colorWaterBodyOutline
        srtmLevel
        imageData
        tileRect
        (forward: ProjectFunc)
        (inverse: InvertFunc)
        (waterBodyOutline: WaterBodyOutline)
        =
        let tileWidth = tileRect.Width

        for localX in 0 .. waterBodyOutline.Raster.Width - 1 do
            for localY in 0 .. waterBodyOutline.Raster.Height - 1 do
                let waterBodyPixelValue =
                    waterBodyOutline.Raster.heightAtLocal (localX, localY)

                if waterBodyPixelValue = 1s then
                    let imagePoint =
                        forward
                            ((localX + waterBodyOutline.Raster.MinX) |> float)
                            ((localY + waterBodyOutline.Raster.MinY) |> float)

                    imagePoint
                    |> Option.iter (fun imagePoint ->
                        let tileX, tileY = imagePoint

                        let pixelValue = Rgba8Bit.rgbaColor 0uy 0uy 0uy 255uy

                        Rgba8Bit.setPixelAt
                            imageData
                            tileWidth
                            (int tileX - tileRect.MinX)
                            (int tileY - tileRect.MinY)
                            pixelValue)
                else
                    ()

    fun heightsArrays srtmLevel (tileRect: Rect) imageData forward inverse ->
        waterBodiesOutlines
        |> Seq.iter (
            colorWaterBodyOutline srtmLevel imageData tileRect forward inverse
        )
