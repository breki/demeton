[<RequireQualifiedAccess>]
module CommandLine.Shell

open CommandLine.Common

let parseAndExecuteCommandLine 
    writeHelpOutput
    executableName
    (args: string[]) 
    supportedCommands = 
    let helpCommand = 
        { 
            HelpCommand.helpCommandTemplateDef 
                with Runner = HelpCommand.run 
                    executableName supportedCommands writeHelpOutput };

    let supportedCommandsIncludingHelp =
        Array.append supportedCommands [| helpCommand |]

    let (commandName, commandArgs) =
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
        | Error _ -> ParsingFailed
    | None -> CommandNotFound
