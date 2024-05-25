[<RequireQualifiedAccess>]
module CommandLine.Arg

open CommandLine.Common

let build name =
    { Name = name
      IsMandatory = true
      Description = ""
      Format = ""
      Example = None
      Parser = fun value -> OkValue value }

let asFloat (arg: CommandArg) =
    { arg with
        Parser = (ValueParsers.parseFloat None) }

let asFloatWithMin minValue (arg: CommandArg) =
    { arg with
        Parser = (ValueParsers.parseFloat (Some minValue)) }

let asPositiveInt (arg: CommandArg) =
    { arg with
        Parser = ValueParsers.parsePositiveInt }

let optional (arg: CommandArg) = { arg with IsMandatory = false }

let desc description (arg: CommandArg) = { arg with Description = description }
let format format (arg: CommandArg) = { arg with Format = format }

let example usage description (arg: CommandArg) =
    { arg with
        Example = Some(usage, description) }

let parser argParser (arg: CommandArg) = { arg with Parser = argParser }

let toPar arg = Arg arg
