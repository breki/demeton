﻿[<RequireQualifiedAccess>]
module Demeton.Shaders.Hillshading

open Demeton.Dem.Types
open Demeton.Shaders.Types
open Demeton.Projections.Common
open Demeton.Geometry.Common
open Demeton.Srtm
open Demeton.Dem.Funcs
open Png

open System
open System.Threading.Tasks

/// <summary>
/// A function that takes a height, slope and aspect and returns a
/// representative color for it.
/// </summary>
type PixelHillshader = float -> float -> float -> Rgba8Bit.RgbaColor

let inline colorComponentRatioToByte (value: float) : byte =
    byte (max (min (int (value * 255.)) 255) 0)

let inline colorComponentRatioToByteLimited
    (byteLimit: byte)
    (value: float)
    : byte =
    byte (max (min (int (value * (float byteLimit))) (int byteLimit)) 0)

let gridSize (coords: LonLat option[]) =
    let lon1, lat1 = Option.get coords.[0]
    let lon2, lat2 = Option.get coords.[1]
    let lon3, lat3 = Option.get coords.[3]

    let width = geodeticDistanceApproximate lon1 lat1 lon2 lat2
    let height = geodeticDistanceApproximate lon1 lat1 lon3 lat3

    (width, height)

let calculatePQ (coords: LonLat option[]) (heights: float option[]) =
    let allHeightsAreAvailable = heights |> Array.forall Option.isSome

    match allHeightsAreAvailable with
    | false -> None
    | true ->
        let heights = heights |> Array.map Option.get

        let gridWidth, gridHeight = gridSize coords

        let p =
            ((heights.[8] + 2. * heights.[5] + heights.[2])
             - (heights.[6] + 2. * heights.[3] + heights.[0]))
            / (8. * gridWidth)

        let q =
            ((heights.[8] + 2. * heights.[7] + heights.[6])
             - (heights.[2] + 2. * heights.[1] + heights.[0]))
            / (8. * gridHeight)

        Some(p, q)

type SlopeAndAspect = float * float

let calculateSlopeAndAspect p q : SlopeAndAspect =
    // a.k.a rise and run
    let maxSlope = Math.Sqrt(p * p + q * q)
    let slope = Math.Atan maxSlope
    //let surfaceInclination = Math.Atan maxSlope
    //let azimuth1 = Math.Asin (-q / surfaceInclination)
    //let azimuth2 = Math.Acos (-p / surfaceInclination)

    let aspect =
        normalizeAngle ((Math.PI * 3. / 2.) - Math.Atan2(q, p)) (Math.PI * 2.)

    (slope, aspect)


/// <summary>
/// Returns a raster shader that uses a specific pixel hillshader.
/// </summary>
let shadeRaster
    dataSourceKey
    tileSize
    (pixelHillshader: PixelHillshader)
    : RasterShader =
    fun dataSources srtmLevel bitmapRect imageData _ inverse ->
        let heightsArray =
            dataSources.FetchDataSource dataSourceKey :?> HeightsArray

        let bitmapWidth = bitmapRect.Width
        let cellsPerDegree = cellsPerDegree tileSize srtmLevel

        let inline lonLatOf x y = inverse (float x) (float y)

        let heightOf (lonRad, latRad) =
            let lonDeg = radToDeg lonRad
            let latDeg = radToDeg latRad

            let globalSrtmX = lonDeg |> longitudeToCellX cellsPerDegree
            let globalSrtmY = latDeg |> latitudeToCellY cellsPerDegree

            heightsArray.interpolateHeightAt (globalSrtmX, globalSrtmY)

        let neighborHeights neighborCoords : float option[] option =
            let allCoordsAreAvailable =
                neighborCoords |> Array.forall Option.isSome

            match allCoordsAreAvailable with
            | true ->
                neighborCoords |> Array.map (Option.get >> heightOf) |> Some
            | false -> None

        /// <summary>
        /// For a specified destination line Y, the function runs a shader
        /// for each pixel of the line.
        /// </summary>
        /// <details>
        /// The function calculates the slope and aspect of the pixel and
        /// feeds it to the shader function. The shader function returns
        /// the pixel value which is then written to the image data.
        /// </details>
        let processRasterLine y =
            for x in bitmapRect.MinX .. (bitmapRect.MaxX - 1) do
                let neighborCoords =
                    [| lonLatOf (x - 1) (y - 1)
                       lonLatOf x (y - 1)
                       lonLatOf (x + 1) (y - 1)
                       lonLatOf (x - 1) y
                       lonLatOf x y
                       lonLatOf (x + 1) y
                       lonLatOf (x - 1) (y + 1)
                       lonLatOf x (y + 1)
                       lonLatOf (x + 1) (y + 1) |]

                let heights = neighborHeights neighborCoords

                heights
                |> Option.bind (calculatePQ neighborCoords)
                |> Option.map (fun (p, q) ->

                    let slope, aspect = calculateSlopeAndAspect p q
                    let height = (Option.get heights).[4] |> Option.get

                    let pixelValue = pixelHillshader height slope aspect

                    Rgba8Bit.setPixelAt
                        imageData
                        bitmapWidth
                        (x - bitmapRect.MinX)
                        // we flip the Y coordinate since DEM heights array
                        // is flipped vertically compared to the bitmap
                        (bitmapRect.MaxY - y - 1)
                        pixelValue)
                |> ignore

        Parallel.For(bitmapRect.MinY, bitmapRect.MaxY, processRasterLine)
        |> ignore
