[<RequireQualifiedAccess>]
module CommandLine.Option

open CommandLine.Common

let build name : CommandOption =
    { Name = name
      Description = ""
      ValuePlaceholder = "value"
      Format = ""
      Default = None
      Example = None
      Parser = fun _ -> OkValue None }

let desc description (opt: CommandOption) =
    { opt with Description = description }

let asFloat minValue (opt: CommandOption) =
    { opt with
        Parser = (ValueParsers.parseFloat minValue)
        ValuePlaceholder = "number"
        Format = sprintf "real number >= %g" minValue }

let asInt (opt: CommandOption) =
    { opt with
        Parser = ValueParsers.parseInt
        ValuePlaceholder = "number"
        Format = "integer value" }

let asDirectory (opt: CommandOption) =
    { opt with
        Parser = ValueParsers.parseDir
        ValuePlaceholder = "path"
        Format = "directory path" }

let asFileName (opt: CommandOption) =
    { opt with
        Parser = ValueParsers.parseFileName
        ValuePlaceholder = "text"
        Format = "text" }

let asPositiveFloat (opt: CommandOption) =
    { opt with
        Parser = ValueParsers.parsePositiveFloat
        ValuePlaceholder = "number"
        Format = "positive real number" }

let asNonNegativeFloat (opt: CommandOption) =
    { opt with
        Parser = (ValueParsers.parseFloat 0.)
        ValuePlaceholder = "number"
        Format = "non-negative real number" }

let asPositiveInt (opt: CommandOption) =
    { opt with
        Parser = ValueParsers.parsePositiveInt
        ValuePlaceholder = "number"
        Format = "positive integer number" }

let format formatDescription (opt: CommandOption) =
    { opt with Format = formatDescription }

let example usage description (opt: CommandOption) =
    { opt with
        Example = Some(usage, description) }

let parser optParser (opt: CommandOption) = { opt with Parser = optParser }

let placeholder text (opt: CommandOption) = { opt with ValuePlaceholder = text }

let defaultValue value (opt: CommandOption) = { opt with Default = value }

let toPar opt = Option opt
