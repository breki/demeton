[<RequireQualifiedAccess>]
module Demeton.Shaders.ElevationColoring

open Raster
open Demeton.DemTypes
open Demeton.Geometry.Common
open Demeton.Srtm
open Demeton.Shaders.Types
open FParsec
open Png
open System.Threading.Tasks
open Text
open Demeton.Srtm.Funcs

type ColorScaleMark = (DemHeight * Rgba8Bit.RgbaColor)

type ColorScale = {
    Marks: ColorScaleMark[]
    NoneColor: Rgba8Bit.RgbaColor
    }

type Parameters = { ColorScale: ColorScale }

let colorScaleToString scale =
    scale.Marks 
    |> Array.fold (fun s (elevation, color) -> 
        s |> appendFormat "{0}:{1};" [| elevation; Rgba8Bit.toHex color |])
        (buildString())
    |> appendFormat "none:{0}" [| scale.NoneColor |> Rgba8Bit.toHex |]
    |> toString

type ParsedMark =
    | Mark of ColorScaleMark
    | NoneColor of Rgba8Bit.RgbaColor

let parseMark: Parser<ParsedMark, unit> =
    pipe4 pint16 (pstring ":") Rgba8Bit.hexColor (pstring ";")
        (fun elevation _ color _ -> Mark (DemHeight elevation, color))

let parseNoneColor: Parser<ParsedMark, unit> =
    pipe3 (pstring "none") (pstring ":") Rgba8Bit.hexColor
        (fun _ _ color -> NoneColor color)

let parseScale: Parser<ColorScale, unit> =
    ((many1 parseMark <?> "invalid color scale") .>>. parseNoneColor)
    |>> fun (marksUntyped, noneColorUntyped) -> 
        let marks = 
            marksUntyped 
            |> List.map (fun x ->
                match x with
                | Mark mark -> mark
                | _ -> invalidOp "bug" )
            |> List.toArray

        let noneColor = 
            match noneColorUntyped with
            | NoneColor color -> color
            | _ -> invalidOp "bug"

        let colorScale: ColorScale = 
            { Marks = marks; NoneColor = noneColor }
        colorScale

let sortScaleMarks marks =
    marks |> Array.sortBy (fun (elevation, _) -> elevation)

let tryParseScale value: Result<ColorScale, string> =
    match value with
    | null -> Result.Error "invalid color scale"
    | _ -> 
        let result = run parseScale value

        match result with
        | Success(scale, _, _) -> 
            let sortedMarks = scale.Marks |> sortScaleMarks
            let marksAreSorted = scale.Marks = sortedMarks
            match marksAreSorted with
            | true -> Result.Ok scale
            | false -> Result.Error "color scale marks are not sorted"

        | _ -> Result.Error "invalid color scale"

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
            0s, Rgba8Bit.rgbColor 204uy 243uy 255uy
            1s, Rgba8Bit.rgbColor 142uy 212uy 142uy
            700s, Rgba8Bit.rgbColor 245uy 250uy 196uy
            1500s, Rgba8Bit.rgbColor 217uy 215uy 189uy
            2500s, Rgba8Bit.rgbColor 242uy 235uy 210uy
            3500s, Rgba8Bit.rgbColor 255uy 255uy 255uy
        |]

        NoneColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 0uy
    }

let defaultParameters = { ColorScale = colorScaleMaperitive }

let shadeRaster
    (colorScale: ColorScale): RasterShader = 
    fun heightsArray srtmLevel tileRect imageData inverse ->

    let cellsPerDegree = cellsPerDegree 3600 srtmLevel 
    
    let tileWidth = tileRect.Width

    let heightForTilePixel x y =
        let lonLatOption = inverse (float x) (float -y)

        match lonLatOption with
        | None -> None
        | Some (lonRad, latRad) ->
            let lonDeg = radToDeg lonRad
            let latDeg = radToDeg latRad

            let globalSrtmX = lonDeg |> longitudeToCellX cellsPerDegree 
            let globalSrtmY = latDeg |> latitudeToCellY cellsPerDegree 
            heightsArray.interpolateHeightAt (globalSrtmX, globalSrtmY)

    let processRasterLine y =
        for x in tileRect.MinX .. (tileRect.MaxX-1) do
            let height = heightForTilePixel x y

            let pixelValue = colorScale |> colorOfHeight height

            Rgba8Bit.setPixelAt 
                imageData
                tileWidth
                (x - tileRect.MinX) 
                (y - tileRect.MinY)
                pixelValue

    Parallel.For(tileRect.MinY, tileRect.MaxY, processRasterLine) |> ignore