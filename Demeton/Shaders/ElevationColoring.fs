module Demeton.Shaders.ElevationColoring

open Demeton.DemTypes
open Png

type ElevationColorScaleMark = (DemHeight * Rgba8Bit.RgbaColor)

type ElevationColorScale = {
    Marks: ElevationColorScaleMark[]
    NoneColor: Rgba8Bit.RgbaColor option
    }

type ElevationColorer = DemHeight -> Rgba8Bit.RgbaColor option

let colorOfHeight (height: DemHeight) (scale: ElevationColorScale) = 
    match height with
    | DemHeightNone -> scale.NoneColor
    | _ -> 
        let markMaybe =
            scale.Marks 
            |> Array.tryFind (fun (markHeight, _) ->  height <= markHeight)
        match markMaybe with
        | Some (_, color) -> Some color
        | None -> 
            let (_, color) = scale.Marks.[scale.Marks.Length - 1]
            Some color


//-32768, GisColor.FromRgb(224, 240, 254)));
//-1, GisColor.FromRgb (142, 212, 142)));
//0, GisColor.FromArgb (0, 142, 212, 142)));
//1, GisColor.FromRgb (142, 212, 142)));
//700, GisColor.FromRgb (245, 250, 196)));
//1500, GisColor.FromRgb (217, 215, 189)));
//2500, GisColor.FromRgb (242, 235, 210)));
//3500, GisColor.FromRgb (255, 255, 255)));


