module Tests.WorldCover.``WorldCover experiments``


open System
open Xunit
open Swensen.Unquote

open Demeton.Commands
open Demeton.Geometry.Common
open Demeton.Dem.Funcs
open Demeton.Projections.PROJParsing
open Demeton.Shaders
open Demeton.WorldCover.Funcs
open Png
open Tests.Shaders
open Tests.WorldCover.RasterSimplification
open Tests.WorldCover.WaterBodiesShaders
open Tests.WorldCover.WaterBodiesColoring
open Tests.WorldCover.WaterBodiesOutlining
open TestHelp


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
let StepNameXcTracerWaterBodies = "XCTracer-water-bodies"

[<Literal>]
let StepNameXcTracerWaterBodiesOutline = "XCTracer-water-bodies-outline"

let hillshadingStep =
    Demeton.Shaders.Pipeline.Common.ShadingStep.HighwireHillshading
        Demeton.Shaders.HighwireHillshader.defaultParameters

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


[<Fact(Skip = "I need to reduce the size of the dataset, it's taking too long to process")>]
let ``Render hillshading with WorldCover water bodies`` () =
    if Environment.GetEnvironmentVariable("CI") = "true" then
        // this test cannot run on CI because we don't have the WorldCover
        // raster available (it's too big to be added to git repo)
        ()
    else
        let worldCoverTileId = demTileXYId -6 -45

        let cacheDir = "cache"

        let result =
            ensureWorldCoverTile
                cacheDir
                FileSys.fileExists
                FileSys.downloadFile
                worldCoverTileId

        test <@ result |> isOk @>

        let waterBodiesHeightsArray =
            readWorldCoverTile cacheDir worldCoverTileId

        let waterBodiesHeightsArray =
            waterBodiesHeightsArray |> convertWorldCoverRasterToWaterMonochrome
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
