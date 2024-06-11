module Demeton.Shaders.Types

open Demeton.Geometry.Common
open Raster
open Demeton.Dem.Types
open Demeton.Projections.Common

// todo 0: create a ShadingDataSources type that stores the data that the shading
// steps work on in a form of a data dictionary and provide a method to retrieve
// the data based on its key

type ShadingDataSources =
    { Sources: Map<string, obj> }

    static member Create() = { Sources = Map.empty }

    member this.FetchDataSource(dataSourceKey: string) =
        match this.Sources.TryGetValue dataSourceKey with
        | true, value -> value
        | _ ->
            invalidArg
                "dataSourceKey"
                $"The data source key '{dataSourceKey}' does not exist in the sources."

    member this.WithDataSource (key: string) (value: obj) =
        { this with
            Sources = this.Sources.Add(key, value) }


/// <summary>
/// A function that fetches data sources used for shading.
/// </summary>
type ShadingDataSourcesFetcher =
    DemLevel
        -> LonLatBounds
        -> ShadingDataSources
        -> Result<ShadingDataSources, string>

let heightsArrayResultToShadingDataSource
    dataSourceKey
    (shadingDataSourcesResult: Result<ShadingDataSources, string>)
    (heightsArrayResult: HeightsArrayMaybeResult)
    : Result<ShadingDataSources, string> =
    shadingDataSourcesResult
    |> Result.bind (fun shadingDataSources ->
        match heightsArrayResult with
        | Ok(Some heightsArray) ->
            shadingDataSources.WithDataSource dataSourceKey heightsArray |> Ok
        | Ok None -> shadingDataSourcesResult
        | Error error -> Error error)



/// <summary>
/// A function that takes a list of heights arrays, a SRTM level,
/// a rectangle specifying the area of the heights array to work on,
/// a raw image data and a map projection forward and inverse
/// functions and applies a shader to the image.
/// </summary>
type RasterShader =
    ShadingDataSources
        -> DemLevel
        -> Rect
        -> RawImageData
        -> ProjectFunc
        -> InvertFunc
        -> unit

[<Literal>]
let DefaultDataSourceKey = "srtm"
