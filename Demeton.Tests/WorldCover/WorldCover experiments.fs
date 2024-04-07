module Tests.WorldCover.``WorldCover experiments``


open System
open Xunit
open Swensen.Unquote

open Demeton.Commands
open Demeton.DemTypes
open Demeton.Geometry.Common
open Demeton.Projections.PROJParsing
open Demeton.Shaders
open Demeton.Srtm.Funcs
open Png
open Tests.Shaders
open Tests.WorldCover.RasterSimplification
open Tests.WorldCover.WaterBodiesShaders
open Tests.WorldCover.WaterBodiesColoring
open Tests.WorldCover.WaterBodiesOutlining
open TestHelp
open WorldCoverRaster


let area, heights, srtmLevel, mapProjection, mapScale, tileRect =
    ShadingSampleGenerator.generateSampleWithParameters
        7.1
        46.1
        7.9
        46.9
        214000.
        72.

let coveragePoints = [ (area.MinLon, area.MinLat); (area.MaxLon, area.MaxLat) ]

[<Literal>]
let StepNameXcTracerHillshading = "XCTracer-hillshading"

[<Literal>]
let StepNameXcTracerWaterBodies = "XCTracer-water-bodies"

[<Literal>]
let StepNameXcTracerWaterBodiesOutline = "XCTracer-water-bodies-outline"

let hillshadingStep = Pipeline.Common.CustomShading StepNameXcTracerHillshading
let waterBodiesStep = Pipeline.Common.CustomShading StepNameXcTracerWaterBodies

let waterBodiesOutlineStep =
    Pipeline.Common.CustomShading StepNameXcTracerWaterBodiesOutline

let hillAndWaterStep =
    Pipeline.Common.Compositing(
        hillshadingStep,
        waterBodiesStep,
        Demeton.Shaders.Pipeline.Common.CompositingFuncIdOver
    )

let outlineOverHillAndWaterStep =
    Pipeline.Common.Compositing(
        hillAndWaterStep,
        waterBodiesOutlineStep,
        Demeton.Shaders.Pipeline.Common.CompositingFuncIdOver
    )


let options: ShadeCommand.Options =
    { CoveragePoints = coveragePoints
      FilePrefix = "XCTracer-hillshading-water"
      LocalCacheDir = "cache"
      OutputDir = "output"
      SrtmDir = "srtm"
      TileSize = 10000
      // RootShadingStep = outlineOverHillAndWaterStep
      RootShadingStep = hillshadingStep
      MapScale = mapScale
      MapProjection =
        { Projection = PROJParameters.Mercator
          IgnoredParameters = [] } }


[<Fact>]
let ``Render hillshading with WorldCover water bodies`` () =
    if Environment.GetEnvironmentVariable("CI") = "true" then
        // this test cannot run on CI because we don't have the WorldCover
        // raster available (it's too big to be added to git repo)
        ()
    else
        let worldCoverData =
            readWorldCoverRaster
                @"C:\temp\WorldCover\ESA_WorldCover_10m_2021_v200_N45E006_Map.tif"
                { Lon = { Value = 6 }
                  Lat = { Value = -45 } }
                { Lon = { Value = 7 }
                  Lat = { Value = -46 } }

        let tileId = parseTileName "N46E007"
        let cellMinX, cellMinY = tileMinCell WorldCoverTileSize tileId

        let waterBodiesHeightsArray =
            HeightsArray(
                cellMinX,
                cellMinY,
                WorldCoverTileSize,
                WorldCoverTileSize,
                HeightsArrayDirectImport worldCoverData
            )
            |> convertWorldCoverRasterToWaterMonochrome
        // |> simplifyRaster 100

        let waterBodies = waterBodiesHeightsArray |> colorWaterBodies

        let waterBodiesOutlines =
            waterBodies
            |> outlineWaterBodies waterBodiesHeightsArray
            |> Seq.toList

        let fetchWorldCoverHeightsArray tileIds =
            waterBodiesHeightsArray |> Some |> Result.Ok

        let heightsArraysFetchers =
            [| Tests.Aw3d.``AW3D experiments``.fetchAw3dHeightsArray
               fetchWorldCoverHeightsArray |]

        let createShaderFunction shaderFunctionName =
            match shaderFunctionName with
            | StepNameXcTracerHillshading ->
                Tests.Aw3d.``AW3D experiments``.xcTracerHillshader
                    IgorHillshader.defaultParameters
                |> Demeton.Shaders.Hillshading.shadeRaster 0
            | StepNameXcTracerWaterBodies ->
                worldCoverWaterBodiesShader 1 waterBodies
            | StepNameXcTracerWaterBodiesOutline ->
                let waterBodiesAndOutlines =
                    List.zip waterBodies waterBodiesOutlines

                worldCoverWaterBodiesOutlineShader waterBodiesAndOutlines
            | _ ->
                failwithf
                    $"Unknown shader function name: %s{shaderFunctionName}"

        let generateTile =
            ShadeCommand.generateShadedRasterTile
                heightsArraysFetchers
                createShaderFunction

        let saveTile =
            ShadeCommand.saveShadedRasterTile
                FileSys.ensureDirectoryExists
                FileSys.openFileToWrite
                File.savePngToStream

        let result = ShadeCommand.run options generateTile saveTile
        test <@ result |> isOk @>
