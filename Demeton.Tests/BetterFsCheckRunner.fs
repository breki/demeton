/// Provides a better FsCheck runner with Xunit integration that can be used
/// directly, not just by using Property attributes and that writes the output
/// using Xunit's ITestOutputHelper.
/// Some code was stolen and remodelled from FsCheck's source code. 
module BetterFsCheckRunner

open FsCheck
open System

let private newline = System.Environment.NewLine
let private pluralize nb = if nb = 1 then String.Empty else "s"

let private labelsToString l = String.concat newline l

let private argumentsToString args =
    let escapeControlChars (s:string) =
        if isNull s then s
        else
            let result = System.Text.StringBuilder()
            let mutable escaped = false
            s |> String.iter (fun ch ->
                    if not (Char.IsControl(ch)) || ch = '\n' || ch = '\r' || ch = '\t' then
                        result.Append(ch) |> ignore
                    else
                        escaped <- true
                        result.Append(ch |> int |> sprintf "\%03i") |> ignore)
            if escaped then result.Append(" (At least one control character has been escaped as a char code, e.g. \\023)") |> ignore
            result.ToString()

    args
    |> List.map (sprintf "%A" >> escapeControlChars)
    |> String.concat newline

let private onStartFixtureToString (t:System.Type) =
    sprintf "--- Checking %s ---%s" t.Name newline

let private maybePrintLabels (l:Set<_>) = 
    match l.Count with
    | 0 -> String.Empty
    | 1 -> sprintf "Label of failing property: %s%s" (labelsToString l) newline
    | _ -> sprintf "Labels of failing property (one or more is failing):%s%s%s" newline (labelsToString l) newline

let private onFailureToString name data originalArgs args usedSeed =
    sprintf "%sFalsifiable, after %i test%s (%i shrink%s) (%A):%s" 
            name data.NumberOfTests (pluralize data.NumberOfTests) data.NumberOfShrinks (pluralize data.NumberOfShrinks) usedSeed newline
        + maybePrintLabels data.Labels
        + sprintf "Original:%s%s%s" newline (argumentsToString originalArgs) newline
        + if (data.NumberOfShrinks > 0 ) then sprintf "Shrunk:%s%s%s" newline (argumentsToString args) newline else ""

///A function that returns the default string that is printed as a result of the test.
let private onFinishedToString name testResult =
    let display l = match l with
                    | []  -> sprintf ".%s" newline
                    | [x] -> sprintf " (%s).%s" x newline
                    | xs  -> sprintf ".%s%s" newline (List.fold (fun acc x -> x + "." + newline + acc) "" xs)
    let entry (p,xs) = sprintf "%A%s %s" p "%" (String.concat ", " xs)
    let stampsToString s = s |> Seq.map entry |> Seq.toList |> display
    let name = if String.IsNullOrEmpty(name) then String.Empty else (name+"-")
    match testResult with
    | TestResult.True (data, suppressOutput) ->
        if suppressOutput then ""
        else sprintf "%sOk, passed %i test%s%s"
                name data.NumberOfTests (pluralize data.NumberOfTests) (data.Stamps |> stampsToString)
    | TestResult.False (data, originalArgs, args, Outcome.Exception exc, usedSeed) -> 
        onFailureToString name data originalArgs args usedSeed
        + sprintf "with exception:%s%O%s" newline exc newline
    | TestResult.False (data, _, args, Outcome.Timeout i, usedSeed) -> 
        sprintf "%sTimeout of %i milliseconds exceeded, after %i test%s (%i shrink%s) (%A):%s" 
            name i data.NumberOfTests (pluralize data.NumberOfTests) data.NumberOfShrinks (pluralize data.NumberOfShrinks) usedSeed newline
        + maybePrintLabels data.Labels 
        + sprintf "%s%s" (args |> argumentsToString) newline
    | TestResult.False (data, originalArgs, args, _, usedSeed) -> 
        onFailureToString name data originalArgs args usedSeed
    | TestResult.Exhausted data -> 
        sprintf "%sArguments exhausted after %i test%s%s" 
            name data.NumberOfTests (pluralize data.NumberOfTests) (data.Stamps |> stampsToString )

/// Creates the BetterFsCheckRunner by providing it Xunit's ITestOutputHelper
/// instance and a verbose boolean flag (if false, the intermediate steps will
/// not be logged).
let betterXUnitRunner
    (output: Xunit.Abstractions.ITestOutputHelper) verbose =
  { new IRunner with
        member x.OnStartFixture t =
            output.WriteLine (onStartFixtureToString t)
        member x.OnArguments (ntest, args, every) =
            if verbose then output.WriteLine (every ntest args)
            else ignore()
        member x.OnShrink(args, everyShrink) =
            output.WriteLine (everyShrink args)
        member x.OnFinished(name,testResult) =
            let resultText = onFinishedToString name testResult
            match testResult with
            | TestResult.True _ -> resultText |> output.WriteLine
            | _ -> failwithf "%s" resultText
  }
