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
        |> ifDo (arg.Example <> None) (fun x -> 
            x 
            |> newLine 
            |> appendFormat "   EXAMPLE: {0} - {1}" [| 
                fst (Option.get arg.Example); snd (Option.get arg.Example) |])
        |> toString

    | Option option ->
        buildString()
        |> appendFormat 
            "{0}{1} <{2}>: {3}" 
            [| ParameterPrefix; option.Name; 
                option.ValuePlaceholder; option.Description  |]
        |> newLine
        |> appendFormat "   FORMAT: {0}" [| option.Format |]
        |> newLine
        |> appendFormat "   DEFAULT VALUE: {0}" [| option.Default |]
        |> ifDo (option.Example <> None) (fun x -> 
            x 
            |> newLine 
            |> appendFormat "   EXAMPLE: {0}{1} {2} - {3}" [| 
                ParameterPrefix; option.Name; 
                fst (Option.get option.Example); snd (Option.get option.Example) |])
        |> toString
    
    | Switch switch ->
        buildString()
        |> appendFormat "{0}{1}: {2}" [| 
            ParameterPrefix; switch.Name; switch.Description |]
        |> toString


[<Fact>]
let ``Can render a command argument description``() =
    let par = 
        Arg { 
            Name = "coverage" 
            Description = "A list of points to be covered."
            Format = "x1,y1,x2,y2..."
            Example = None
            Parser = fun _ -> OkValue 1 
        }

    test <@ parameterDescription par = 
        @"<coverage>: A list of points to be covered.
   FORMAT: x1,y1,x2,y2..." @>

[<Fact>]
let ``Can render a command argument description with an example``() =
    let par = 
        Arg { 
            Name = "coverage" 
            Description = "A list of points to be covered."
            Format = "x1,y1,x2,y2..."
            Example = Some 
                ("5,43.3,16.6,48.4", "fetches (roughly) the whole Alps area")
            Parser = fun _ -> OkValue 1 
        }

    test <@ parameterDescription par = 
        @"<coverage>: A list of points to be covered.
   FORMAT: x1,y1,x2,y2...
   EXAMPLE: 5,43.3,16.6,48.4 - fetches (roughly) the whole Alps area" @>

[<Fact>]
let ``Can render an option description``() =
    let par = 
        Option { 
            Name = "dpi" 
            Description = 
                "The printing resolution required for the resulting raster image."
            ValuePlaceholder = "number"
            Format = "positive real number"
            Default = 300.
            Example = None
            Parser = fun _ -> OkValue 1 
        }

    test <@ parameterDescription par = 
        @"--dpi <number>: The printing resolution required for the resulting raster image.
   FORMAT: positive real number
   DEFAULT VALUE: 300"
   @>

[<Fact>]
let ``Can render an option description with an example``() =
    let par = 
        Option { 
            Name = "dpi" 
            Description = 
                "The printing resolution required for the resulting raster image."
            ValuePlaceholder = "number"
            Format = "positive real number"
            Default = 300.
            Example = Some ("1200", "specifies the printing resolution of 1200 dots per inch")
            Parser = fun _ -> OkValue 1 
        }

    test <@ parameterDescription par = 
        @"--dpi <number>: The printing resolution required for the resulting raster image.
   FORMAT: positive real number
   DEFAULT VALUE: 300
   EXAMPLE: --dpi 1200 - specifies the printing resolution of 1200 dots per inch"
   @>

[<Fact>]
let ``Can render a switch description``() =
    let par = 
        Switch { 
            Name = "verbose" 
            Description = "Turns on the verbose logging." }

    test <@ parameterDescription par = 
        @"--verbose: Turns on the verbose logging."
   @>

