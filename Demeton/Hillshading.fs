module Demeton.Hillshading

open Demeton.Geometry.Common
open Demeton.Srtm.Funcs
open System;
open System.IO

type Color = { A: byte; R: byte; G: byte; B: byte }

type ShaderParameters = 
    { 
        SunAzimuth: float
        ShadingIntensity: float 
        ShadingColorR: byte
        ShadingColorG: byte
        ShadingColorB: byte
    }


let normalizeAngle (angle: float) (normalizer: float) =
    let angleRemainder = angle % normalizer
    match angleRemainder with
    | x when x < 0. -> angleRemainder + normalizer
    | _ -> angleRemainder


let differenceBetweenAngles (angle1: float) (angle2: float) (normalizer: float) =
    let diff = normalizeAngle angle1 normalizer 
                - normalizeAngle angle2 normalizer
    let diffAbs = abs diff

    if diffAbs > normalizer / 2. then normalizer - diffAbs
    else diffAbs


let colorComponentRatioToByte (value: float): byte =
    (byte)(max (min ((int)(value * 255.)) 255) 0)


let igorHillshade 
        (parameters: ShaderParameters) 
        (elevation: float) 
        (aspect: float) 
        (slope: float)
        : Color =
    let aspectDiff = differenceBetweenAngles
                        aspect 
                        (Math.PI * 3./2. - parameters.SunAzimuth)
                        (Math.PI * 2.)
    
    let slopeNormalized = max slope 0.

    let slopeDegrees = min (slopeNormalized / Math.PI * 180.) 90.

    let slopeStrength = slopeDegrees / 90.
    let aspectStrength = 1. - aspectDiff / Math.PI
    let shadowness = slopeStrength * aspectStrength

    let alpha = colorComponentRatioToByte 
                    (shadowness * parameters.ShadingIntensity)
    
    { 
        A = alpha
        R = parameters.ShadingColorR
        G = parameters.ShadingColorG
        B = parameters.ShadingColorB
    }


let windowMinMax (window: float []): (float * float) option =
    let min (v: float option): float option =
        //match v with
        //| None -> None
        //|

        invalidOp "todo"
        //window |> Array.minBy (fun x -> matchx )

    invalidOp "todo"

let aspectSlope 
        (cellSizeInMeters: float)
        (window: float option [,])
        : (float * float) option =

    match window.[1,1] with
    | None -> None
    | _ -> 
            
    invalidOp "todo"

// todo: implement hillshade
let hillshade (bounds: LonLatBounds): Stream option =
    let neededTiles = boundsToTiles bounds

    None
