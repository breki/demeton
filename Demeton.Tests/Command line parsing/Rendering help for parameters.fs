module Tests.``Command line parsing``.``Rendering help for parameters``

open CommandLine
open CommandLine.Common

open Xunit
open Swensen.Unquote

[<Fact>]
let ``Can render a command argument description``() =
    let par = 
        Arg.build "coverage" 
        |> Arg.desc "A list of points to be covered."
        |> Arg.format "x1,y1,x2,y2..."
        |> Arg.toPar

    test <@ HelpCommand.parameterDescription par = 
        @"<coverage>: A list of points to be covered.
   FORMAT: x1,y1,x2,y2..." @>

[<Fact>]
let ``Can render a command argument description with an example``() =
    let par = 
        Arg.build "coverage" 
        |> Arg.desc "A list of points to be covered."
        |> Arg.format "x1,y1,x2,y2..."
        |> Arg.example 
            "5,43.3,16.6,48.4" "fetches (roughly) the whole Alps area"
        |> Arg.toPar

    test <@ HelpCommand.parameterDescription par = 
        @"<coverage>: A list of points to be covered.
   FORMAT: x1,y1,x2,y2...
   EXAMPLE: 5,43.3,16.6,48.4 - fetches (roughly) the whole Alps area" @>

[<Fact>]
let ``Can render an option description``() =
    let par = 
        Option.build "dpi" 
        |> Option.desc "The printing resolution required for the resulting raster image."
        |> Option.placeholder "number"
        |> Option.format "positive real number"
        |> Option.defaultValue 300.
        |> Option.toPar

    test <@ HelpCommand.parameterDescription par = 
        @"--dpi <number>: The printing resolution required for the resulting raster image.
   FORMAT: positive real number
   DEFAULT VALUE: 300"
   @>

[<Fact>]
let ``Can render an option description with an example``() =
    let par = 
        Option.build "dpi" 
        |> Option.desc "The printing resolution required for the resulting raster image."
        |> Option.placeholder "number"
        |> Option.format "positive real number"
        |> Option.defaultValue 300.
        |> Option.example "1200" "specifies the printing resolution of 1200 dots per inch"
        |> Option.toPar

    test <@ HelpCommand.parameterDescription par = 
        @"--dpi <number>: The printing resolution required for the resulting raster image.
   FORMAT: positive real number
   DEFAULT VALUE: 300
   EXAMPLE: --dpi 1200 - specifies the printing resolution of 1200 dots per inch"
   @>

[<Fact>]
let ``Can render a switch description``() =
    let par = 
        Switch.build "verbose" |> Switch.desc "Turns on the verbose logging." 
        |> Switch.toPar

    test <@ HelpCommand.parameterDescription par = 
        @"--verbose: Turns on the verbose logging."
   @>

