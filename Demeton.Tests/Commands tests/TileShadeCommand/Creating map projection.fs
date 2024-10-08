﻿module Tests.Commands_tests.TileShadeCommand.Creating_map_projection

open Demeton.Commands

open Demeton.Shaders
open Xunit
open Demeton.Geometry.Common

[<Fact>]
let ``Projection is created`` () =
    let options: TileShadeCommand.Options =
        { TileWidth = 100
          TileHeight = 200
          TileCenter = (10, 20)
          HgtSize = 1200
          MapScale = Some (TileShadeCommand.MapScaleOf 250000)
          Dpi = 245
          LambertHillshadingIntensity = 1.
          IgorHillshadingIntensity = 1.
          SlopeShadingIntensity = 1.
          SunAzimuth = IgorHillshader.DefaultSunAzimuth |> degToRad
          SunAltitude = LambertHillshader.DefaultSunAltitude |> degToRad
          WaterBodiesColor = "#49C8FF" |> Png.Rgba8Bit.parseColorHexValue
          LocalCacheDir = TileShadeCommand.DefaultLocalCacheDir
          OutputDir = TileShadeCommand.DefaultOutputDir
          OutputFileName = TileShadeCommand.DefaultOutputFileName }

    let projection = TileShadeCommand.createProjection options

    match projection with
    | Ok projection -> ()
    | Error error -> invalidOp error
