module Demeton.Shaders.WaterBodies.DataSources

open Demeton.Geometry.Common
open Demeton.Dem.Funcs
open Demeton.Projections.Common
open Demeton.Shaders.Types
open Demeton.WorldCover.Types
open Demeton.WorldCover.Funcs
open Demeton.WorldCover.Fetch
open Demeton.WorldCover.WaterBodiesColoring
open Raster

[<Literal>]
let WaterBodiesHeightsArrayDataSourceKey = "waterBodiesRaster"

[<Literal>]
let WaterBodiesColoredListDataSourceKey = "waterBodiesColoredList"

[<Literal>]
let WaterBodiesOutlinesDataSourceKey = "waterBodiesOutlines"

// todo sometime 2: we need tests for readWorldCoverTiffFile
let fetchWorldCoverHeightsArray
    (mapProjection: MapProjection)
    cacheDir
    demLevel
    (coverageArea: LonLatBounds)
    =
    let coveragePoints =
        [ (coverageArea.MinLon, coverageArea.MinLat)
          (coverageArea.MaxLon, coverageArea.MaxLat) ]

    let fileDownloadingResult = ensureWorldCoverFiles cacheDir coverageArea

    match fileDownloadingResult with
    | Ok tilesIds ->
        let filesHeightsArrays =
            tilesIds
            |> Seq.map (readWorldCoverTiffFile cacheDir None)
            |> Seq.toList

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

        let cellsPerDegree = WorldCoverTileSize

        // now convert lon, lat to DEM coordinates
        let coveragePointsInDemCoords =
            deprojectedCoveragePoints
            |> List.map (fun (lon, lat) ->
                let cellX = lon |> radToDeg |> longitudeToCellX (float cellsPerDegree)
                let cellY = lat |> radToDeg |> latitudeToCellY (float cellsPerDegree)
                (cellX, cellY))

        let demMbr = Demeton.Geometry.Bounds.mbrOf coveragePointsInDemCoords

        // a buffer around the DEM MBR so we don't end up outside of the array
        // when we calculate the heights
        let safetyBuffer = 50

        let mergedArrayBounds =
            Rect.asMinMax
                ((demMbr.MinX |> floor |> int) - safetyBuffer)
                ((demMbr.MinY |> floor |> int) - safetyBuffer)
                ((demMbr.MaxX |> ceil |> int) + safetyBuffer)
                ((demMbr.MaxY |> ceil |> int) + safetyBuffer)

        merge mergedArrayBounds filesHeightsArrays |> Result.Ok
    | Error message -> Result.Error message


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
    | Ok(Some waterBodiesHeightsArray) ->
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
        // let waterBodiesOutlines =
        //     waterBodies
        //     |> outlineWaterBodies waterBodiesHeightsArray
        //     |> Seq.toList

        // let dataSources =
        //     dataSources.WithDataSource
        //         WaterBodiesOutlinesDataSourceKey
        //         waterBodiesOutlines

        dataSources |> Ok
    | _ -> dataSources |> Ok
