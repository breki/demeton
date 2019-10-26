[<RequireQualifiedAccess>]
module Demeton.Shaders.ElevationColoring

open Raster
open Demeton.DemTypes
open Demeton.Geometry.Common
open Demeton.Projections
open Demeton.Srtm
open Demeton.Shaders.Types
open Png

type ColorScaleMark = (DemHeight * Rgba8Bit.RgbaColor)

type ColorScale = {
    Marks: ColorScaleMark[]
    NoneColor: Rgba8Bit.RgbaColor
    }

let colorOfHeight (heightMaybe: float option) (scale: ColorScale) = 
    let findColor (height: float): Rgba8Bit.RgbaColor =
        let mutable color = None
        let mutable markIndex = 0;
        while Option.isNone color && markIndex < scale.Marks.Length do
            let (markHeight, markColor) = scale.Marks.[markIndex]

            if height <= float markHeight then
                if markIndex = 0 then
                    color <- Some markColor
                else
                    let (prevMarkHeight, prevMarkColor) = 
                        scale.Marks.[markIndex - 1]

                    color <- 
                        let mixRatio = 
                            (height - float prevMarkHeight)
                            / float (markHeight - prevMarkHeight)
                        Some (Rgba8Bit.mixColors 
                            prevMarkColor markColor mixRatio)
            else
                if markIndex = scale.Marks.Length - 1 then
                    color <- Some markColor

                markIndex <- markIndex + 1

        Option.get color

    match heightMaybe with
    | None -> scale.NoneColor
    | Some height -> findColor height


/// <summary>
/// A elevation color scale used in Maperitive program.
/// </summary>
let colorScaleMaperitive =
    {
        Marks = [| 
            -1s, Rgba8Bit.rgbColor 142uy 212uy 142uy
            0s, Rgba8Bit.rgbaColor 142uy 212uy 142uy 0uy
            1s, Rgba8Bit.rgbColor 142uy 212uy 142uy
            700s, Rgba8Bit.rgbColor 245uy 250uy 196uy
            1500s, Rgba8Bit.rgbColor 217uy 215uy 189uy
            2500s, Rgba8Bit.rgbColor 242uy 235uy 210uy
            3500s, Rgba8Bit.rgbColor 255uy 255uy 255uy
        |]

        NoneColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 0uy
    }

// todo the function should accept the color scale as a parameter
let shadeRaster (colorScale: ColorScale): RasterShader = 
    fun heightsArray tileRect imageData options ->

    let tileWidth = tileRect.Width
    let scaleFactor = options.ProjectionScaleFactor

    let heightForTilePixel x y =
        let xUnscaled = float x / scaleFactor
        let yUnscaled = float y / scaleFactor
        let lonLatOption = WebMercator.inverse xUnscaled yUnscaled

        match lonLatOption with
        | None -> None
        | Some (lonRad, latRad) ->
            let lonDeg = radToDeg lonRad
            let latDeg = radToDeg latRad

            let globalSrtmX = Tile.longitudeToGlobalX lonDeg 3600
            let globalSrtmY = Tile.latitudeToGlobalY latDeg 3600
            heightsArray.interpolateHeightAt (globalSrtmX, globalSrtmY)

    for y in tileRect.MinY .. (tileRect.MaxY-1) do
        for x in tileRect.MinX .. (tileRect.MaxX-1) do
            let height = heightForTilePixel x y

            let pixelValue = colorScale |> colorOfHeight height

            Rgba8Bit.setPixelAt 
                imageData
                tileWidth
                (x - tileRect.MinX) 
                (y - tileRect.MinY)
                pixelValue
