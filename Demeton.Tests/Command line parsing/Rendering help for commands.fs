module Tests.``Command line parsing``.``Rendering help for commands``

open CommandLine
open CommandLine.Common

open Xunit
open Swensen.Unquote

let parametersDescriptions (parameters: CommandParameter[]) =
    parameters 
    |> Array.map (fun p -> p |> HelpCommand.parameterDescription) 
    |> String.concat "\n\n"

let supportedParameters = [|
    Arg { 
        Name = "arg1"
        Description = "some description 1"
        Format = "format1"
        Example = None
        Parser = fun _ -> OkValue 1 }
    Arg { 
        Name = "arg2"
        Description = "some description 2"
        Format = "format2"
        Example = Some ("x1", "xx1")
        Parser = fun _ -> OkValue 1 }
    Switch {
        Name = "switch1"
        Description = "some description 3" }
    Option {
        Name = "option1" 
        Description = "some description 4"
        ValuePlaceholder = "value"
        Format = "format3"
        Default = 1
        Example = None
        Parser = fun _ -> OkValue 1 }
    |]

[<Fact(Skip="todo")>]
let ``Puts a newline between two descriptions``() =
    let pars = [|
        Arg { 
            Name = "arg1"
            Description = "some description 1"
            Format = "format1"
            Example = None
            Parser = fun _ -> OkValue 1 }
        Switch {
            Name = "switch1"
            Description = "some description 3" }
        |]

    test <@ parametersDescriptions pars =
        @"<arg1>: some description 1
   FORMAT: format1

--switch1: some description 3" @>
