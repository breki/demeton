module Demeton.Shaders.WaterBodies.WaterBodiesShaders

open System
open System.Threading.Tasks

open Demeton.Dem.Types
open Demeton.Geometry.Common
open Demeton.Projections.Common
open Demeton.Shaders.Types
open Demeton.Dem.Funcs
open Demeton.WorldCover.Types
open Demeton.WorldCover.WaterBodiesColoring
open Demeton.WorldCover.WaterBodiesOutlining
open Png
open Raster


[<Literal>]
let StepNameWaterBodies = "water-bodies"

[<Literal>]
let StepNameWaterBodiesOutline = "water-bodies-outline"


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
/// <param name="waterBodiesHeightsArrayDataSourceKey">
/// The key of the data source that contains the water bodies heights array.
/// </param>
/// <param name="waterBodiesColoredListDataSourceKey">
/// The key of the data source that contains the water bodies colored list.
/// </param>
/// <param name="waterColor">
/// The color to use for rendering water bodies.
/// </param>
/// <param name="debugMode">
/// Whether to render ignored or error water body pixels in their separate
/// colors. If false, ignored and error water body pixels will be rendered
/// as transparent, like the rest of the non-water body pixels.
/// </param>
let worldCoverWaterBodiesShader
    waterBodiesHeightsArrayDataSourceKey
    waterBodiesColoredListDataSourceKey
    waterColor
    debugMode
    : RasterShader =
    fun dataSources demLevel heightsArrayTargetArea imageData forward inverse ->
        let cellsPerDegree = cellsPerDegree WorldCoverTileSize demLevel

        let waterBodiesHeightsArray =
            dataSources.FetchDataSource(waterBodiesHeightsArrayDataSourceKey)
            :?> HeightsArray

        let waterBodiesColoredList =
            dataSources.FetchDataSource(waterBodiesColoredListDataSourceKey)
            :?> WaterBody list

        let targetAreaWidth = heightsArrayTargetArea.Width

        let ignoredWaterColor = Rgba8Bit.parseColorHexValue "#FF96D1"
        let errorWaterColor = Rgba8Bit.parseColorHexValue "#FF4800"

        let noWaterColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 0uy

        let processRasterLine y =
            for x in
                heightsArrayTargetArea.MinX .. (heightsArrayTargetArea.MaxX - 1) do
                let rasterValue =
                    waterBodiesHeightsArray
                    |> valueForProjectedPixel x y cellsPerDegree inverse

                let pixelValue =
                    match rasterValue with
                    | None -> noWaterColor
                    | Some 0s -> noWaterColor
                    | Some 1s when debugMode -> errorWaterColor
                    | Some waterBodyColor ->
                        let waterBody =
                            waterBodiesColoredList.[int waterBodyColor - 2]

                        if shouldShowWaterBody waterBody then
                            waterColor
                        elif debugMode then
                            ignoredWaterColor
                        else
                            noWaterColor

                Rgba8Bit.setPixelAt
                    imageData
                    targetAreaWidth
                    (x - heightsArrayTargetArea.MinX)
                    // we flip the Y coordinate since DEM heights array
                    // is flipped vertically compared to the bitmap
                    (heightsArrayTargetArea.MaxY - y - 1)
                    pixelValue

        Parallel.For(
            heightsArrayTargetArea.MinY,
            heightsArrayTargetArea.MaxY,
            processRasterLine
        )
        |> ignore

let worldCoverWaterBodiesOutlineShader
    waterBodiesColoredListDataSourceKey
    waterBodiesOutlinesDataSourceKey
    : RasterShader =

    let colorWaterBodyOutline
        demLevel
        imageData
        heightsArrayTargetArea
        (forward: ProjectFunc)
        (inverse: InvertFunc)
        ((waterBody, waterBodyOutline): WaterBody * WaterBodyOutline)
        =
        if shouldShowWaterBody waterBody then
            let cellsPerDegree = cellsPerDegree WorldCoverTileSize demLevel

            let targetAreaWidth = heightsArrayTargetArea.Width

            let outlinePixelColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 255uy

            // for each pixel of the water body outline raster
            for localX in 0 .. waterBodyOutline.Raster.Width - 1 do
                for localY in 0 .. waterBodyOutline.Raster.Height - 1 do
                    let outlineDistance =
                        waterBodyOutline.Raster.heightAtLocal (localX, localY)

                    // if the pixel indicates an outline
                    if outlineDistance >= 1s && outlineDistance <= 3s then
                        // project the raster pixel into the coordinate space
                        // of the image
                        let lon =
                            (waterBodyOutline.Raster.MinX + localX)
                            |> float
                            |> cellXToLongitude cellsPerDegree
                            |> degToRad

                        let lat =
                            (waterBodyOutline.Raster.MinY + localY)
                            |> float
                            |> cellYToLatitude cellsPerDegree
                            |> degToRad

                        let projectedPoint = forward lon lat

                        // if the projected point is valid...
                        projectedPoint
                        |> Option.iter (fun (tileX, tileY) ->
                            let imageX =
                                (tileX |> Math.Round |> int)
                                - heightsArrayTargetArea.MinX

                            // we flip the Y coordinate since DEM heights array
                            // is flipped vertically compared to the bitmap
                            let imageY =
                                heightsArrayTargetArea.MaxY
                                - (tileY |> Math.Round |> int)

                            // ... and within the tile image,
                            // render a black pixel at the projected point
                            if
                                imageX >= 0
                                && imageX < heightsArrayTargetArea.Width
                                && imageY >= 0
                                && imageY < heightsArrayTargetArea.Height
                            then
                                Rgba8Bit.setPixelAt
                                    imageData
                                    targetAreaWidth
                                    imageX
                                    imageY
                                    outlinePixelColor)
                    else
                        ()
        else
            ()

    fun dataSources demLevel (tileRect: Rect) imageData forward inverse ->
        let waterBodiesColoredList =
            dataSources.FetchDataSource(waterBodiesColoredListDataSourceKey)
            :?> WaterBody list

        let waterBodiesAndOutlines =
            dataSources.FetchDataSource(waterBodiesOutlinesDataSourceKey)
            :?> WaterBodyOutline list

        let waterBodiesAndOutlines =
            waterBodiesAndOutlines |> List.zip waterBodiesColoredList

        waterBodiesAndOutlines
        |> Seq.iter (
            colorWaterBodyOutline demLevel imageData tileRect forward inverse
        )
