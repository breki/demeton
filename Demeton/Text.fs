/// <summary>
/// Contains functions for building a string using 
/// <see cref="System.Text.StringBuilder" />.
/// </summary>
module Text

open System
open System.Text

/// <summary>
/// Starts building of a string by creating a 
/// <see cref="System.Text.StringBuilder" /> instance.
/// </summary>
let buildString() = StringBuilder()

/// <summary>
/// Appends a text to the
/// <see cref="System.Text.StringBuilder" /> instance.
/// </summary>
let append (text: string) (sb: StringBuilder) =
    sb.Append(text)

/// <summary>
/// Appends a character
/// <see cref="System.Text.StringBuilder" /> instance.
/// </summary>
let appendChar (chr: char) (sb: StringBuilder) =
    sb.Append(chr)

let appendLine (text: string) (sb: StringBuilder) =
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

let inline (+@) (a: string) (b: string) = 
    a + Environment.NewLine + Environment.NewLine + b

