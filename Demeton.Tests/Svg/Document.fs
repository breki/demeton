[<RequireQualifiedAccess>]
module Svg.Document

open Svg.DocumentModel
let build width height viewBox children =
    { Style = None; Classes = None; Width = width; Height = height;
      ViewBox = viewBox; Children = children }