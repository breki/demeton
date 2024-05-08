[<RequireQualifiedAccess>]
module Demeton.Commands.TileShadeCommand

open CommandLine
open CommandLine.Common
open Demeton.Geometry.Common

type Options =
    { TileWidth: int
      TileHeight: int
      TileCenter: LonLat
      PixelSize: float option
      MapScale: float option
      WaterBodiesColor: Png.Rgba8Bit.ArgbColor }

[<Literal>]
let TileWidthParameter = "tile-width"

[<Literal>]
let TileHeightParameter = "tile-height"

[<Literal>]
let CenterLongitudeParameter = "center-longitude"

[<Literal>]
let CenterLatitudeParameter = "center-latitude"

[<Literal>]
let PixelSizeParameter = "pixel-size"

[<Literal>]
let MapScaleParameter = "map-scale"

[<Literal>]
let WaterBodiesColor = "water-color"

let defaultWaterBodiesColor = "#49C8FF" |> Png.Rgba8Bit.parseColorHexValue

let parseColorParameter value =
    match Png.Rgba8Bit.tryParseColorHexValue value with
    | Ok color -> OkValue color
    | Error error -> InvalidValue error.Message

let supportedParameters: CommandParameter[] =
    [| Arg.build TileWidthParameter
       |> Arg.desc "The width of the tile (in pixels)."
       |> Arg.asPositiveInt
       |> Arg.toPar

       Arg.build TileHeightParameter
       |> Arg.desc "The height of the tile (in pixels)."
       |> Arg.asPositiveInt
       |> Arg.toPar

       Arg.build CenterLongitudeParameter
       |> Arg.desc "The longitude of the center of the tile."
       |> Arg.asFloat -180
       |> Arg.toPar

       Arg.build CenterLatitudeParameter
       |> Arg.desc "The latitude of the center of the tile."
       |> Arg.asFloat -90
       |> Arg.toPar

       Option.build PixelSizeParameter
       |> Option.desc "The size of the pixel (in meters)."
       |> Option.asPositiveFloat
       |> Option.defaultValue 20
       |> Option.toPar

       Option.build MapScaleParameter
       |> Option.desc "The map scale."
       |> Option.asPositiveFloat
       |> Option.toPar

       Option.build WaterBodiesColor
       |> Option.desc "The color of the water bodies."
       |> Option.example "#49C8FF" "uses a light blue color for water bodies"
       |> Option.parser parseColorParameter
       |> Option.defaultValue defaultWaterBodiesColor
       |> Option.toPar |]

let fillOptions parsedParameters =
    let defaultOptions =
        { TileWidth = 100
          TileHeight = 100
          TileCenter = (0., 0.)
          PixelSize = None
          MapScale = None
          WaterBodiesColor = defaultWaterBodiesColor }

    let processParameter options parameter =
        match parameter with
        | ParsedArg { Name = TileWidthParameter
                      Value = value } ->
            { options with
                TileWidth = value :?> int }
        | ParsedArg { Name = TileHeightParameter
                      Value = value } ->
            { options with
                TileHeight = value :?> int }
        | ParsedArg { Name = CenterLongitudeParameter
                      Value = value } ->
            let lon = value :?> float

            { options with
                TileCenter = (lon, options.TileCenter |> snd) }
        | ParsedArg { Name = CenterLatitudeParameter
                      Value = value } ->
            let lat = value :?> float

            { options with
                TileCenter = (options.TileCenter |> fst, lat) }
        | ParsedOption { Name = PixelSizeParameter
                         Value = value } ->
            { options with
                PixelSize = Some(value :?> float) }
        | ParsedOption { Name = MapScaleParameter
                         Value = value } ->
            { options with
                MapScale = Some(value :?> float) }
        | ParsedOption { Name = WaterBodiesColor
                         Value = value } ->
            { options with
                WaterBodiesColor = value :?> Png.Rgba8Bit.ArgbColor }
        | _ -> invalidOp "Unrecognized parameter."

    let filledOptions =
        parsedParameters |> List.fold processParameter defaultOptions

    let filledOptions =
        match filledOptions.PixelSize, filledOptions.MapScale with
        | Some _, Some _ ->
            invalidOp "Cannot specify both pixel size and map scale."
        | None, None ->
            { filledOptions with
                PixelSize = Some 20. }
        | _ -> filledOptions

    filledOptions

let run (options: Options) : Result<unit, string> =
    Result.Error "not implemented yet"
