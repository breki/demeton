[<RequireQualifiedAccess>]
module Demeton.Shaders.SolidBackground

open System.Threading.Tasks
open Demeton.Shaders.Types
open Png

type Parameters = { BackgroundColor: Rgba8Bit.RgbaColor }

let shadeRaster (backgroundColor: Rgba8Bit.RgbaColor) : RasterShader =
    fun _ _ tileRect imageData _ _ ->
        let tileWidth = tileRect.Width

        let processRasterLine y =
            for x in tileRect.MinX .. (tileRect.MaxX - 1) do
                Rgba8Bit.setPixelAt
                    imageData
                    tileWidth
                    (x - tileRect.MinX)
                    (y - tileRect.MinY)
                    backgroundColor

        Parallel.For(tileRect.MinY, tileRect.MaxY, processRasterLine) |> ignore
