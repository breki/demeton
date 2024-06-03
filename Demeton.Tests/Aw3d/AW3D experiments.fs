module Tests.Aw3d.``AW3D experiments``


open Demeton.Commands
open Demeton.Geometry.Common
open Demeton.Projections.PROJParsing
open Demeton.Shaders
open FsUnit
open Png
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
            [| TileShadeCommand.fetchAw3dHeightsArray mapProjection CacheDir |]
            createShaderFunction

    let saveTile =
        ShadeCommand.saveShadedRasterTile
            FileSys.ensureDirectoryExists
            FileSys.openFileToWrite
            File.savePngToStream

    let result = ShadeCommand.run options generateTile saveTile
    test <@ result |> isOk @>
