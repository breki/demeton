﻿module Text

open System.Text

let buildString() = StringBuilder()

let appendLine text (sb: StringBuilder) =
    sb.AppendLine(text)

let appendLines lines (sb: StringBuilder) =
    lines |> Seq.fold (fun builder line -> builder |> appendLine line) sb

let appendFormat 
    format 
    ([<System.ParamArray>] args: obj []) 
    (sb: StringBuilder) 
    = 
    sb.AppendFormat (format, args)

let newLine (sb: StringBuilder) = sb.AppendLine()

let ifDo condition work (sb: StringBuilder) =
    if condition then sb |> work
    else sb

let toString (sb: StringBuilder) = sb.ToString()