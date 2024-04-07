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
/// A filter function that, based on the water body's surface area and coverage
/// ratio, determines whether the water body should be shown or not.
/// </summary>
let shouldShowWaterBody waterBody =
    let totalArea = waterBody.Coverage.Width * waterBody.Coverage.Height

    match waterBody.SurfaceArea, totalArea / waterBody.SurfaceArea with
    | surfaceArea, _ when surfaceArea < 1250 -> false
    | _, coverageRatio when coverageRatio >= 10 -> false
    | _ -> true


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

                        if shouldShowWaterBody waterBody then
                            waterColor
                        else
                            ignoredWaterColor

                Rgba8Bit.setPixelAt
                    imageData
                    tileWidth
                    (x - tileRect.MinX)
                    (y - tileRect.MinY)
                    pixelValue

        Parallel.For(tileRect.MinY, tileRect.MaxY, processRasterLine) |> ignore

let worldCoverWaterBodiesOutlineShader
    (waterBodiesAndOutlines: (WaterBody * WaterBodyOutline) list)
    : RasterShader =

    let colorWaterBodyOutline
        srtmLevel
        imageData
        tileRect
        (forward: ProjectFunc)
        (inverse: InvertFunc)
        ((waterBody, waterBodyOutline): WaterBody * WaterBodyOutline)
        =
        if shouldShowWaterBody waterBody then
            let tileWidth = tileRect.Width

            let outlinePixelColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 255uy

            // for each pixel of the water body outline raster
            for localX in 0 .. waterBodyOutline.Raster.Width - 1 do
                for localY in 0 .. waterBodyOutline.Raster.Height - 1 do
                    let outlineDistance =
                        waterBodyOutline.Raster.heightAtLocal (localX, localY)

                    // if the pixel indicates an outline
                    if outlineDistance >= 1s && outlineDistance <= 3s then
                        // project the raster pixel into the coordinate space of the
                        // tile image
                        let lon =
                            (waterBodyOutline.Raster.MinX + localX)
                            |> float
                            |> cellXToLongitude (float WorldCoverTileSize)
                            |> degToRad

                        let lat =
                            (waterBodyOutline.Raster.MinY + localY)
                            |> float
                            |> cellYToLatitude (float WorldCoverTileSize)
                            |> degToRad

                        let projectedPoint = forward lon lat

                        // if the projected point is valid...
                        projectedPoint
                        |> Option.iter (fun (tileX, tileY) ->
                            let imageX =
                                (tileX |> Math.Round |> int) - tileRect.MinX

                            let imageY =
                                (-tileY |> Math.Round |> int) - tileRect.MinY

                            // ... and within the tile image,
                            // render a black pixel at the projected point
                            if
                                imageX >= 0
                                && imageX < tileRect.Width
                                && imageY >= 0
                                && imageY < tileRect.Height
                            then
                                Rgba8Bit.setPixelAt
                                    imageData
                                    tileWidth
                                    imageX
                                    imageY
                                    outlinePixelColor)
                    else
                        ()
        else
            ()

    fun heightsArrays srtmLevel (tileRect: Rect) imageData forward inverse ->
        waterBodiesAndOutlines
        |> Seq.iter (
            colorWaterBodyOutline srtmLevel imageData tileRect forward inverse
        )
