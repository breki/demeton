module Demeton.Shaders.ElevationColoring

open Demeton.DemTypes
open Png

type ElevationColorScaleMark = (DemHeight * Rgba8Bit.RgbaColor)

type ElevationColorScale = {
    Marks: ElevationColorScaleMark[]
    NoneColor: Rgba8Bit.RgbaColor option
    }

type ElevationColorer = DemHeight -> Rgba8Bit.RgbaColor option

let colorOfHeight (heightMaybe: float option) (scale: ElevationColorScale) = 
    let findColor (height: float): Rgba8Bit.RgbaColor option =
        let mutable color = None
        let mutable markIndex = 0;
        while Option.isNone color && markIndex < scale.Marks.Length do
            let (markHeight, markColor) = scale.Marks.[markIndex]

            if height <= float markHeight then
                if markIndex = 0 then
                    color <- Some markColor
                else
                    let (prevMarkHeight, prevMarkColor) = 
                        scale.Marks.[markIndex - 1]

                    color <- 
                        let mixRatio = 
                            (height - float prevMarkHeight)
                            / float (markHeight - prevMarkHeight)
                        Some (Rgba8Bit.mixColors 
                            prevMarkColor markColor mixRatio)
            else
                if markIndex = scale.Marks.Length - 1 then
                    color <- Some markColor

                markIndex <- markIndex + 1

        color

    match heightMaybe with
    | None -> scale.NoneColor
    | Some height -> findColor height

//-32768, GisColor.FromRgb(224, 240, 254)));
//-1, GisColor.FromRgb (142, 212, 142)));
//0, GisColor.FromArgb (0, 142, 212, 142)));
//1, GisColor.FromRgb (142, 212, 142)));
//700, GisColor.FromRgb (245, 250, 196)));
//1500, GisColor.FromRgb (217, 215, 189)));
//2500, GisColor.FromRgb (242, 235, 210)));
//3500, GisColor.FromRgb (255, 255, 255)));


