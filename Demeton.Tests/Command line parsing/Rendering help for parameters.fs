module Tests.``Command line parsing``.``Rendering help for parameters``

open CommandLine
open CommandLine.Common

open Xunit
open Swensen.Unquote

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

    test <@ HelpCommand.parameterDescription par = 
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

    test <@ HelpCommand.parameterDescription par = 
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

    test <@ HelpCommand.parameterDescription par = 
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

    test <@ HelpCommand.parameterDescription par = 
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

    test <@ HelpCommand.parameterDescription par = 
        @"--verbose: Turns on the verbose logging."
   @>

