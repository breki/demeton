[<RequireQualifiedAccess>]
module Demeton.Shaders.Hillshading

open Demeton.Shaders.Types
open Demeton.Projections
open Demeton.Projections.Common
open Demeton.Geometry.Common
open Demeton.Srtm
open Png

open System

type PixelHillshader = float -> float -> float -> Rgba8Bit.RgbaColor

let colorComponentRatioToByte (value: float): byte =
    (byte)(max (min ((int)(value * 255.)) 255) 0)

let gridSize (coords: LonLat option[]) =
    let (lon1, lat1) = Option.get coords.[0]
    let (lon2, lat2) = Option.get coords.[1]
    let (lon3, lat3) = Option.get coords.[3]

    let width = geodeticDistanceApproximate lon1 lat1 lon2 lat2
    let height = geodeticDistanceApproximate lon1 lat1 lon3 lat3

    (width, height)

let calculatePQ 
    (coords: LonLat option[]) (heightsMaybe: float option[] option) =
    match heightsMaybe with
    | None -> None
    | Some heights ->
        let allHeightsAreAvailable = heights |> Array.forall Option.isSome

        match allHeightsAreAvailable with
        | false -> None
        | true ->
            let heights = 
                heightsMaybe |> Option.get |> Array.map Option.get

            let (gridWidth, gridHeight) = gridSize coords

            let p = 
                ((heights.[8] + 2. * heights.[5] + heights.[2])
                - (heights.[6] + 2. * heights.[3] + heights.[0]))
                / (8. * gridWidth)

            let q =
                ((heights.[8] + 2. * heights.[7] + heights.[6])
                - (heights.[2] + 2. * heights.[1] + heights.[0]))
                / (8. * gridHeight)
                
            Some (p, q)

type SlopeAndAspect = float * float

let calculateSlopeAndAspect p q: SlopeAndAspect =
    // a.k.a rise and run
    let maxSlope = Math.Sqrt (p * p + q * q)
    let slope = Math.Atan maxSlope
    //let surfaceInclination = Math.Atan maxSlope
    //let azimuth1 = Math.Asin (-q / surfaceInclination)
    //let azimuth2 = Math.Acos (-p / surfaceInclination)
    let aspect = Math.Atan2 (-q, p)

    (slope, aspect)


let shadeRaster (pixelHillshader: PixelHillshader): RasterShader = 
    fun heightsArray tileRect imageData options ->

    let scaleFactor = options.ProjectionScaleFactor
    let tileWidth = tileRect.Width

    let lonLatOf x y =
        let xUnscaled = float x / scaleFactor
        let yUnscaled = float y / scaleFactor
        WebMercator.inverse xUnscaled yUnscaled

    let heightOf (lonRad, latRad) =
        let lonDeg = radToDeg lonRad
        let latDeg = radToDeg latRad

        let globalSrtmX = Tile.longitudeToGlobalX lonDeg 3600
        let globalSrtmY = Tile.latitudeToGlobalY latDeg 3600
        heightsArray.interpolateHeightAt (globalSrtmX, globalSrtmY)

    let neighborHeights neighborCoords: float option[] option =
        let allCoordsAreAvailable = 
            neighborCoords |> Array.forall Option.isSome

        match allCoordsAreAvailable with
        | true -> 
            neighborCoords 
            |> Array.map (fun heightMaybe -> heightOf (Option.get heightMaybe))
            |> Some
        | false -> None

    for y in tileRect.MinY .. (tileRect.MaxY-1) do
        for x in tileRect.MinX .. (tileRect.MaxX-1) do
            let neighborCoords = [|
                lonLatOf (x-1) (y-1)
                lonLatOf x (y-1)
                lonLatOf (x+1) (y-1)
                lonLatOf (x-1) y
                lonLatOf x y
                lonLatOf (x+1) y
                lonLatOf (x-1) (y+1)
                lonLatOf x (y+1)
                lonLatOf (x+1) (y+1)
            |]

            let heights = neighborHeights neighborCoords

            let pqMaybe = calculatePQ neighborCoords heights

            match pqMaybe with
            | Some (p, q) -> 
                let (slope, aspect) = calculateSlopeAndAspect p q           
                let height = (Option.get heights).[4] |> Option.get

                let pixelValue = pixelHillshader height slope aspect 

                Rgba8Bit.setPixelAt 
                    imageData
                    tileWidth
                    (x - tileRect.MinX) 
                    (y - tileRect.MinY)
                    pixelValue

            | None -> ignore()

