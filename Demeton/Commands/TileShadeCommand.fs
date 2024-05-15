[<RequireQualifiedAccess>]
module Demeton.Commands.TileShadeCommand

open System
open CommandLine
open CommandLine.Common
open Demeton.Geometry.Common
open Demeton.Projections
open Demeton.Projections.Common
open Demeton.Projections.PROJParsing
open Demeton.Aw3d.Funcs
open Demeton.WorldCover.Funcs
open FileSys

// todo 10: add cache dir parameter

type Options =
    { TileWidth: int
      TileHeight: int
      TileCenter: LonLat
      PixelSize: float option
      MapScale: float option
      Dpi: float
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
let DpiParameter = "dpi"

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

       Option.build DpiParameter
       |> Option.desc "The DPI of the output image (the default is 254 DPI)."
       |> Option.asPositiveFloat
       |> Option.defaultValue 254.
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
          Dpi = 254.
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
        | ParsedOption { Name = DpiParameter; Value = value } ->
            { options with Dpi = value :?> float }
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

let ensureAw3dTiles cacheDir (bounds: LonLatBounds) : Result<unit, string> =
    Log.info "Ensuring all needed AW3D tiles are there..."

    let aw3dTilesNeeded = bounds |> boundsToAw3dTiles |> Seq.toList

    let aw3dTileResults =
        aw3dTilesNeeded
        |> List.map (fun tileId ->
            tileId,
            ensureAw3dTile
                cacheDir
                fileExists
                downloadFile
                readZipFile
                copyStreamToFile
                deleteFile
                tileId)

    let aw3dErrors =
        aw3dTileResults
        |> List.choose (fun (_, result) ->
            match result with
            | Ok _ -> None
            | Error message -> Some message)

    match aw3dErrors with
    | [] -> Result.Ok()
    | _ -> Result.Error(String.concat "\n" aw3dErrors)

let ensureWorldCoverTiles
    cacheDir
    (bounds: LonLatBounds)
    : Result<unit, string> =
    Log.info "Ensuring all needed WorldCover tiles are there..."

    let geoJsonFile = ensureGeoJsonFile cacheDir fileExists downloadFile

    let allAvailableTiles = listAllAvailableTiles openFileToRead geoJsonFile

    let tilesNeeded = bounds |> boundsToWorldCoverTiles |> Seq.toList

    let availableTilesNeeded =
        tilesNeeded
        |> List.filter (fun tileId ->
            allAvailableTiles
            |> Seq.exists (fun availableTileId -> availableTileId = tileId))

    let tilesResults =
        availableTilesNeeded
        |> List.map (fun tileId ->
            tileId,
            ensureWorldCoverTile cacheDir fileExists downloadFile tileId)

    let tilesErrors =
        tilesResults
        |> List.choose (fun (_, result) ->
            match result with
            | Ok _ -> None
            | Error message -> Some message)

    match tilesErrors with
    | [] -> Result.Ok()
    | _ -> Result.Error(String.concat "\n" tilesErrors)

let generateHillshadingTile cacheDir bounds =
    Log.info "Generating hillshading tile..."

    Result.Ok()

let run (options: Options) : Result<unit, string> =
    let centerLon, centerLat = options.TileCenter
    // we invert the latitude since in our internal system, + is north
    let centerLat = -centerLat

    // https://desktop.arcgis.com/en/arcmap/latest/map/projections/lambert-conformal-conic.htm
    let projectionParameters: LambertConformalConic.Parameters =
        { X0 = centerLon
          Y0 = centerLat
          Lon0 = centerLon
          Lat0 = centerLat
          // todo 50: we set standard parallels to be the same as the center, for now
          Lat1 = centerLat
          Lat2 = centerLat
          K0 = 1.
          Ellipsoid = WGS84 }

    let mapScaleValue =
        match options.MapScale with
        | Some mapScale -> mapScale
        | None ->
            raise (
                NotImplementedException(
                    "Pixel size support not implemented yet"
                )
            )

    let mapScale: MapScale =
        { MapScale = mapScaleValue
          Dpi = options.Dpi }

    let projection =
        Factory.createMapProjection
            (LambertConformalConic projectionParameters)
            mapScale

    match projection with
    | Ok projection ->
        let halfWidth = options.TileWidth / 2
        let halfHeight = options.TileHeight / 2

        let tileBoundingPoints: (int * int)[] =
            [| (-1, -1); (1, -1); (1, 1); (-1, 1) |]

        let tileBoundingGeoPointsMaybe =
            tileBoundingPoints
            |> Array.map (fun (x, y) ->
                let x = x * halfWidth
                let y = y * halfHeight

                projection.Invert x y)

        let tileBoundingGeoPoints =
            tileBoundingGeoPointsMaybe |> Array.choose id

        if
            tileBoundingGeoPoints |> Array.length = (tileBoundingPoints
                                                     |> Array.length)
        then
            let tileBoundingGeoPoints =
                tileBoundingGeoPoints
                |> Array.map (fun (lon, lat) -> (radToDeg lon, radToDeg lat))

            let geoAreaNeeded =
                Demeton.Geometry.Bounds.mbrOf tileBoundingGeoPoints
                |> lonLatBoundsFromBounds

            Log.info
                "Geo area needed: minLon: %f, minLat: %f, maxLon: %f, maxLat: %f"
                geoAreaNeeded.MinLon
                -geoAreaNeeded.MinLat
                geoAreaNeeded.MaxLon
                -geoAreaNeeded.MaxLat

            let cacheDir = "cache"

            match ensureAw3dTiles cacheDir geoAreaNeeded with
            | Ok _ ->
                match ensureWorldCoverTiles cacheDir geoAreaNeeded with
                | Ok _ -> generateHillshadingTile cacheDir geoAreaNeeded
                | Error message -> Result.Error message
            | Error message -> Result.Error message
        else
            Result.Error
                "Some of the tile bounding points could not be projected."
    | Error message -> Result.Error message
