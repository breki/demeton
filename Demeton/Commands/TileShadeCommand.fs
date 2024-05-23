[<RequireQualifiedAccess>]
module Demeton.Commands.TileShadeCommand

open System
open CommandLine
open CommandLine.Common
open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Demeton.Geometry.Common
open Demeton.Projections
open Demeton.Projections.Common
open Demeton.Projections.PROJParsing
open Demeton.Aw3d.Types
open Demeton.Aw3d.Funcs
open Demeton.Shaders
open FileSys
open Png
open Png.Types
open Raster
open Demeton.Shaders.Pipeline.Common

// todo 30: add cache dir parameter

type Options =
    { TileWidth: int
      TileHeight: int
      TileCenter: LonLat
      PixelSize: float option
      MapScale: float option
      Dpi: float
      WaterBodiesColor: Rgba8Bit.ArgbColor }

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

let defaultWaterBodiesColor = "#49C8FF" |> Rgba8Bit.parseColorHexValue

let parseColorParameter value =
    match Rgba8Bit.tryParseColorHexValue value with
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
                WaterBodiesColor = value :?> Rgba8Bit.ArgbColor }
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

let createProjection options =
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

    let centerLon, centerLat = options.TileCenter

    // https://desktop.arcgis.com/en/arcmap/latest/map/projections/lambert-conformal-conic.htm
    let projectionParameters: LambertConformalConic.Parameters =
        { X0 = 0
          Y0 = 0
          Lon0 = centerLon
          Lat0 = centerLat
          // todo 50: we set standard parallels to be the same as the center, for now
          Lat1 = centerLat
          Lat2 = centerLat
          K0 = 1.
          Ellipsoid = WGS84 }

    Factory.createMapProjection
        (LambertConformalConic projectionParameters)
        mapScale

// Factory.createMapProjection Mercator mapScale



let calculateGeoAreaMbr options projection =
    let tileBoundingPoints: (int * int)[] =
        [| (-1, -1); (1, -1); (1, 1); (-1, 1) |]

    let halfWidth = options.TileWidth / 2
    let halfHeight = options.TileHeight / 2

    let tileBoundingGeoPoints =
        tileBoundingPoints
        |> Array.map (fun (x, y) ->
            let x = x * halfWidth
            let y = y * halfHeight

            projection.Invert x -y)
        |> Array.choose id
        |> Array.map (fun (lon, lat) -> (radToDeg lon, radToDeg lat))

    if
        tileBoundingGeoPoints |> Array.length = (tileBoundingPoints
                                                 |> Array.length)
    then
        Demeton.Geometry.Bounds.mbrOf tileBoundingGeoPoints
        |> lonLatBoundsFromBounds
        |> Ok
    else
        Result.Error "Some of the tile bounding points could not be projected."


let fetchAw3dHeightsArray
    mapProjection
    cacheDir
    coverageArea
    (tilesIds: DemTileId seq)
    =
    let coveragePoints =
        [ (coverageArea.MinLon, coverageArea.MinLat)
          (coverageArea.MaxLon, coverageArea.MaxLat) ]

    let tileDownloadingResult = ensureAw3dTiles cacheDir coverageArea

    match tileDownloadingResult with
    | Ok tilesIds ->
        let tilesHeightsArrays =
            tilesIds |> Seq.map (readAw3dTile cacheDir) |> Seq.toList

        // calculate mergedArrayBounds for the given area
        let projectedCoveragePoints =
            coveragePoints
            |> List.map (fun (lon, lat) ->
                mapProjection.Proj (lon |> degToRad) (lat |> degToRad))
            |> List.choose id

        let deprojectedCoveragePoints =
            projectedCoveragePoints
            |> List.map (fun (x, y) -> mapProjection.Invert x y)
            |> List.choose id

        let cellsPerDegree = Aw3dTileSize

        // now convert lon, lat to DEM coordinates
        let coveragePointsInDemCoords =
            deprojectedCoveragePoints
            |> List.map (fun (lon, lat) ->
                let cellX = lon |> radToDeg |> longitudeToCellX cellsPerDegree
                let cellY = lat |> radToDeg |> latitudeToCellY cellsPerDegree
                (cellX, cellY))

        let demMbr = Demeton.Geometry.Bounds.mbrOf coveragePointsInDemCoords

        // a buffer around the DEM MBR so we don't end up outside of the array
        // when we calculate the heights
        let safetyBuffer = 5

        let mergedArrayBounds =
            Rect.asMinMax
                ((demMbr.MinX |> floor |> int) - safetyBuffer)
                ((demMbr.MinY |> floor |> int) - safetyBuffer)
                ((demMbr.MaxX |> ceil |> int) + safetyBuffer)
                ((demMbr.MaxY |> ceil |> int) + safetyBuffer)

        merge mergedArrayBounds tilesHeightsArrays |> Result.Ok
    | Error message -> Result.Error message


let saveTileFile
    (ensureDirectoryExists: DirectoryExistsEnsurer)
    (openFileToWrite: FileWriter)
    (writePngToStream: File.PngStreamWriter)
    (tileRect: Rect)
    imageData
    =
    // todo 10: add OutputDir to options
    let outputDir = "output"

    ensureDirectoryExists outputDir |> ignore

    let tilePngFileName = outputDir |> Pth.combine ("tile.png")

    openFileToWrite tilePngFileName
    |> Result.map (fun stream ->
        let ihdr: IhdrData =
            { Width = tileRect.Width
              Height = tileRect.Height
              BitDepth = PngBitDepth.BitDepth8
              ColorType = PngColorType.RgbAlpha
              InterlaceMethod = PngInterlaceMethod.NoInterlace }

        stream |> writePngToStream ihdr imageData |> closeStream

        Log.info $"Saved the tile to %s{tilePngFileName}"

        tilePngFileName)
    |> Result.mapError fileSysErrorMessage


let run (options: Options) : Result<unit, string> =
    let cacheDir = "cache"
    let srtmLevel: DemLevel = { Value = 0 }

    // the center of the raster rect should represent the center specified in
    // the options
    let tileRect =
        { MinX = -options.TileWidth / 2
          MinY = -options.TileHeight / 2
          Width = options.TileWidth
          Height = options.TileHeight }

    let solidBackgroundStepParameters: SolidBackground.Parameters =
        { BackgroundColor = Rgba8Bit.parseColorHexValue "#FFFFFF" }

    let solidBackgroundStep =
        ShadingStep.SolidBackground solidBackgroundStepParameters

    let igorHillshadingStep =
        ShadingStep.IgorHillshading IgorHillshader.defaultParameters

    let igorHillshadingStep =
        ShadingStep.IgorHillshading IgorHillshader.defaultParameters

    let slopeShadingStep =
        ShadingStep.SlopeShading SlopeShader.defaultParameters

    let hillshadingStep =
        Compositing(
            igorHillshadingStep,
            slopeShadingStep,
            CompositingFuncIdAlphaDarken
        )

    let rootShadingStep =
        Compositing(solidBackgroundStep, hillshadingStep, CompositingFuncIdOver)

    match createProjection options with
    | Ok mapProjection ->
        match calculateGeoAreaMbr options mapProjection with
        | Ok coverageArea ->
            ShadeCommand.generateShadedRasterTile
                [| fetchAw3dHeightsArray mapProjection cacheDir coverageArea |]
                createShadingFuncById
                srtmLevel
                tileRect
                rootShadingStep
                mapProjection
            |> Result.bind (fun imageData ->
                match imageData with
                | Some imageData ->
                    Log.info "Saving the tile..."

                    saveTileFile
                        ensureDirectoryExists
                        openFileToWrite
                        File.savePngToStream
                        tileRect
                        imageData
                    |> ignore

                    Ok()
                | None -> Error "No image data generated.")
        | Error message -> Result.Error message
    | Error message -> Result.Error message
