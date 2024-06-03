module Tests.``Commands tests``.ShadeCommand.``Generating shaded tile``

open Demeton.Geometry.Common
open Raster
open Demeton.Commands
open Demeton.Shaders

open Xunit
open Swensen.Unquote
open TestHelp
open Tests.Shaders

let area, heights, srtmLevel, mapProjection, mapScale, tileRect =
    ShadingSampleGenerator.generateSampleWithParameters
        4.262676
        42.90816
        16.962471
        48.502048
        5000000.
        100.

let coveragePoints = [ (area.MinLon, area.MinLat); (area.MaxLon, area.MaxLat) ]

let mockRasterShader _ _ _ _ _ _ = ()

let rootShadingStep = Pipeline.Common.CustomShading "some shader"

[<Fact>]
let ``Tile generator correctly specifies DEM level and bounds for tile downloaders``
    ()
    =

    let correctLevelAndAreaWereProvided level area =
        test <@ level = srtmLevel @>

        test <@ area.MinLon |> isApproxEqualTo 4.249 (Decimals 3) @>
        test <@ area.MinLat |> isApproxEqualTo 42.900 (Decimals 3) @>
        test <@ area.MaxLon |> isApproxEqualTo 16.984 (Decimals 3) @>
        test <@ area.MaxLat |> isApproxEqualTo 48.508 (Decimals 3) @>

        heights |> Some |> Ok

    let result =
        ShadeCommand.generateShadedRasterTile
            [| correctLevelAndAreaWereProvided |]
            (fun _ -> mockRasterShader)
            srtmLevel
            tileRect
            rootShadingStep
            mapProjection

    test <@ result |> isOk @>

    test <@ true @>

[<Fact>]
let ``When heights array fetcher returns None, tile generator does nothing and returns None``
    ()
    =

    let returnNoneForHeightsArray _ _ = Ok None

    let shadeTileResult =
        ShadeCommand.generateShadedRasterTile
            [| returnNoneForHeightsArray |]
            (fun _ -> mockRasterShader)
            srtmLevel
            tileRect
            rootShadingStep
            mapProjection

    test <@ isOk shadeTileResult @>
    test <@ shadeTileResult |> isOkValue None @>

[<Fact>]
let ``When heights array fetcher returns an error, tile generator returns an error, too``
    ()
    =

    let returnErrorInsteadOfHeightsArray _ _ = Error "something is wrong"

    let shadeTileResult =
        ShadeCommand.generateShadedRasterTile
            [| returnErrorInsteadOfHeightsArray |]
            (fun _ -> mockRasterShader)
            srtmLevel
            tileRect
            rootShadingStep
            mapProjection

    test <@ isError shadeTileResult @>

[<Fact>]
let ``Tile generator prepares the tile image data and returns it`` () =
    let fetchSomeHeights _ _ = heights |> Some |> Ok

    let mutable imageDataReceived = None

    let shadeRasterReceivesTileRectAndImageData
        _
        _
        tileRectReceived
        (imageData: RawImageData)
        _
        _
        =
        imageDataReceived <- Some imageData

        test
            <@
                imageData.Length = tileRect.Width
                                   * tileRect.Height
                                   * Png.Rgba8Bit.BytesPerPixel
            @>

        test <@ tileRectReceived = tileRect @>

    let result =
        ShadeCommand.generateShadedRasterTile
            [| fetchSomeHeights |]
            (fun _ -> shadeRasterReceivesTileRectAndImageData)
            srtmLevel
            tileRect
            (Pipeline.Common.CustomShading "whatever")
            mapProjection

    test <@ result = Ok imageDataReceived @>
