module Tests.Aw3d.``AW3D experiments``


open Demeton.Commands
open Demeton.Geometry.Common
open Demeton.Dem.Funcs
open Demeton.Projections.PROJParsing
open Demeton.Shaders
open Demeton.Aw3d.Types
open Demeton.Aw3d.Funcs
open FsUnit
open Png
open Raster
open Tests.Shaders
open Xunit
open TestHelp
open Swensen.Unquote

[<Literal>]
let CacheDir = "cache"

let area, heights, srtmLevel, mapProjection, mapScale, tileRect =
    ShadingSampleGenerator.generateSampleWithParameters
        7.416765
        46.613756
        7.628785
        46.652998
        25000.
        72.

let coveragePoints = [ (area.MinLon, area.MinLat); (area.MaxLon, area.MaxLat) ]

let options: ShadeCommand.Options =
    { CoveragePoints = coveragePoints
      FilePrefix = "shading"
      LocalCacheDir = CacheDir
      OutputDir = "output"
      SrtmDir = "srtm"
      TileSize = 10000
      RootShadingStep = Pipeline.Common.CustomShading "XCTracer"
      MapScale = mapScale
      MapProjection =
        { Projection = PROJParameters.Mercator
          IgnoredParameters = [] } }


// todo 10: move this to the command module?
let fetchAw3dHeightsArray _ =
    let tileDownloadingResult = ensureAw3dTiles CacheDir area

    match tileDownloadingResult with
    | Ok tilesIds ->
        let tilesHeightsArrays =
            tilesIds |> Seq.map (readAw3dTile CacheDir) |> Seq.toList

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

        let mergedArrayBounds =
            Rect.asMinMax
                ((demMbr.MinX |> floor |> int) - 1)
                ((demMbr.MinY |> floor |> int) - 1)
                ((demMbr.MaxX |> ceil |> int) + 1)
                ((demMbr.MaxY |> ceil |> int) + 1)

        merge mergedArrayBounds tilesHeightsArrays |> Result.Ok
    | Error message -> Result.Error message


[<Fact(Skip = "downloads the tile so it takes too long")>]
let ``Generate hillshading from AW3D`` () =
    let pixelShader =
        IgorHillshader.shadePixel
            { SunAzimuth = IgorHillshader.DefaultSunAzimuth |> degToRad
              ShadingColor = 0u
              Intensity = 1.
              HeightsArrayIndex = 0 }

    let createShaderFunction _ =
        Demeton.Shaders.Hillshading.shadeRaster 0 pixelShader

    let generateTile =
        ShadeCommand.generateShadedRasterTile
            [| fetchAw3dHeightsArray |]
            createShaderFunction

    let saveTile =
        ShadeCommand.saveShadedRasterTile
            FileSys.ensureDirectoryExists
            FileSys.openFileToWrite
            File.savePngToStream

    let result = ShadeCommand.run options generateTile saveTile
    test <@ result |> isOk @>
