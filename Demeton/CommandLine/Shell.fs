[<RequireQualifiedAccess>]
module CommandLine.Shell

open CommandLine.Common

let tryFindCommand commandName supportedCommands =
    supportedCommands |> Array.tryFind (fun cmd -> cmd.Name = commandName)

let parseAndExecuteCommandLine 
    writeHelpOutput
    executableName
    (args: string[]) 
    supportedCommands = 
    let helpCommand = 
        { 
            HelpCommand.helpCommandTemplateDef 
                with Runner = HelpCommand.runCommand 
                    executableName supportedCommands writeHelpOutput };

    let supportedCommandsIncludingHelp =
        Array.append supportedCommands [| helpCommand |]

    let commandName = args.[0]
    let commandMaybe = 
        supportedCommandsIncludingHelp |> tryFindCommand commandName

    match commandMaybe with
    | Some command -> 
        let commandArgs = args |> Array.tail |> Array.toList

        let parsingResult = parseParameters commandArgs command.Parameters
        match parsingResult with
        | Ok parsedParameters -> command.Runner parsedParameters
        | Error _ -> ParsingFailed
    | None -> CommandNotFound
