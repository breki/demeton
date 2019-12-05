[<RequireQualifiedAccess>]
module Svg.Group

open Svg.DocumentModel

let build (groupName: ElementName) (children: GroupChildElement list) =
    { Id = groupName; Style = None; Classes = None; Children = children }

let withClass (className: ClassName) (group: Group) =
    { group with Classes = Some [ className ] }