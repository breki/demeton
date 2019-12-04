[<RequireQualifiedAccess>]
module Svg.SvgLength

open Svg.DocumentModel

let noUnit number = { Unit = None; Number = number }
let mm number = { Unit = Some Mm; Number = number }

let toString length =
    let numberStr =
        length.Number.ToString
            ("0.##", System.Globalization.CultureInfo.InvariantCulture)
    
    match length.Unit with
    | None -> numberStr
    | Some Mm -> numberStr + "mm"