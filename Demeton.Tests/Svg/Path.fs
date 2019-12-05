[<RequireQualifiedAccess>]
module Svg.Path

open Svg.DocumentModel

let build (pathName: ElementName) (pathData: PathData): Path =
    { Id = pathName; Style = None; Classes = None; PathData = pathData }
