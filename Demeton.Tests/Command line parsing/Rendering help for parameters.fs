module Tests.``Command line parsing``.``Rendering help for parameters``

open CommandLine
open CommandLine.Common
open Text

open Xunit
open Swensen.Unquote

let parameterDescription parameter: string =
    match parameter with
    | Arg arg -> 
        buildString()
        |> appendFormat "<{0}>: {1}" [| arg.Name; arg.Description |]
        |> newLine
        |> appendFormat "   FORMAT: {0}" [| arg.Format |]
        |> toString
    | _ -> invalidOp "todo"

[<Fact>]
let ``Can render a command argument description``() =
    let par = 
        Arg { 
            Name = "coverage" 
            Description = "A list of points to be covered."
            Format = "x1,y1,x2,y2..."
            Parser = fun _ -> OkValue 1 
        }

    test <@ parameterDescription par = 
        @"<coverage>: A list of points to be covered.
   FORMAT: x1,y1,x2,y2..." @>

