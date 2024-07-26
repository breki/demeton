module Tests.WorldCover.``WorldCover experiments``


open System
open Xunit
open Swensen.Unquote

open Demeton.Commands
open Demeton.Geometry.Common
open Demeton.Projections.PROJParsing
open Demeton.Shaders
open Demeton.Shaders.Types
open Demeton.WorldCover.Funcs
open Demeton.WorldCover.WaterBodiesColoring
open Demeton.WorldCover.WaterBodiesOutlining
open Demeton.Shaders.WaterBodies.DataSources
open Demeton.Shaders.WaterBodies.WaterBodiesShaders
open Png

open Tests.Shaders
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

let hillshadingStep =
    Demeton.Shaders.Pipeline.Common.ShadingStep.IgorHillshading
        { IgorHillshader.defaultParameters with
            DataSourceKey = "aw3d" }

let waterBodiesStep = Pipeline.Common.CustomShading StepNameWaterBodies

let waterBodiesOutlineStep =
    Pipeline.Common.CustomShading StepNameWaterBodiesOutline

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
      RootShadingStep = hillAndWaterStep
      MapScale = mapScale
      MapProjection =
        { Projection = PROJParameters.Mercator
          IgnoredParameters = [] } }


let fetchWaterBodiesDataSources
    mapProjection
    cacheDir
    level
    coverageArea
    (dataSources: ShadingDataSources)
    =
    let waterBodiesHeightsArrayResult =
        fetchWorldCoverHeightsArray mapProjection cacheDir level coverageArea

    // if we actually got the water bodies heights array, we can calculate
    // the derived data sources from it
    match waterBodiesHeightsArrayResult with
    | Some waterBodiesHeightsArray ->
        let waterBodiesHeightsArray =
            waterBodiesHeightsArray |> convertWorldCoverRasterToWaterMonochrome
        // |> simplifyRaster 100

        let dataSources =
            dataSources.WithDataSource
                WaterBodiesHeightsArrayDataSourceKey
                waterBodiesHeightsArray

        // construct water bodies data source and add it to the sources
        let waterBodies = waterBodiesHeightsArray |> colorWaterBodies

        let dataSources =
            dataSources.WithDataSource
                WaterBodiesColoredListDataSourceKey
                waterBodies

        // construct water bodies outline data source and add it to the sources
        let waterBodiesOutlines =
            waterBodies
            |> outlineWaterBodies waterBodiesHeightsArray
            |> Seq.toList

        let dataSources =
            dataSources.WithDataSource
                WaterBodiesOutlinesDataSourceKey
                waterBodiesOutlines

        dataSources |> Ok
    | _ -> dataSources |> Ok

// [<Fact(Skip = "Work in progress")>]
[<Fact>]
let ``Render hillshading with WorldCover water bodies`` () =
    if Environment.GetEnvironmentVariable("CI") = "true" then
        // this test cannot run on CI because we don't have the WorldCover
        // raster available (it's too big to be added to git repo)
        ()
    else
        let cacheDir = "cache"
        let waterColor = Rgba8Bit.parseColorHexValue "#49C8FF"

        let shadingDataSourcesFetchers =
            [| fun level coverageArea dataSources ->
                   TileShadeCommand.fetchAw3dHeightsArray
                       mapProjection
                       cacheDir
                       level
                       coverageArea
                   |> heightsArrayResultToShadingDataSource
                       "aw3d"
                       (Ok dataSources)
               fetchWaterBodiesDataSources mapProjection cacheDir |]

        let waterBodiesDebugMode = true

        let generateTile =
            ShadeCommand.generateShadedRasterTile
                shadingDataSourcesFetchers
                (TileShadeCommand.createShaderFunction waterColor waterBodiesDebugMode)

        let saveTile =
            ShadeCommand.saveShadedRasterTile
                FileSys.ensureDirectoryExists
                FileSys.openFileToWrite
                File.savePngToStream

        let result = ShadeCommand.run options generateTile saveTile
        test <@ result |> isOk @>
