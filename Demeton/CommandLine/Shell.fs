[<RequireQualifiedAccess>]
module CommandLine.Shell

open System
open CommandLine.Common

let parseAndExecuteCommandLine
    writeHelpOutput
    (writeErrorOutput: string -> unit)
    executableName
    (args: string[])
    supportedCommands
    =
    let helpCommand =
        { HelpCommand.helpCommandTemplateDef with
            Runner =
                HelpCommand.run
                    executableName
                    supportedCommands
                    writeHelpOutput
                    writeErrorOutput }

    let supportedCommandsIncludingHelp =
        Array.append supportedCommands [| helpCommand |]

    let commandName, commandArgs =
        match args |> Array.toList with
        | [] -> ("help", [])
        | commandName :: commandArgs -> (commandName, commandArgs)

    let commandMaybe =
        supportedCommandsIncludingHelp |> tryFindCommand commandName

    match commandMaybe with
    | Some command ->
        let parsingResult = parseParameters commandArgs command.Parameters

        match parsingResult with
        | Ok parsedParameters -> command.Runner parsedParameters
        | Error message ->
            writeErrorOutput message
            writeErrorOutput Environment.NewLine
            ParsingFailed

    | None ->
        sprintf
            "Unrecognized command '%s'. Please use 'help' command to list all available commands."
            commandName
        |> writeErrorOutput

        writeErrorOutput Environment.NewLine
        UnrecognizedCommand
