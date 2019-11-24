module Tests.Projections.``PROJ parsing``

open Xunit
open Swensen.Unquote

// https://proj.org/usage/projections.html
// https://github.com/OSGeo/PROJ/blob/master/schemas/v0.2/projjson.schema.json
// https://proj.org/operations/projections/index.html#projections

type ProjSpec = string

let parseProj projSpec = invalidOp "todo"

[<Fact(Skip="todo")>]
let ``Parse a single parameter``() =
    test <@ parseProj "+a" @>
    