/// <summary>
/// Provides a function to execute multiple functions that return Result and 
/// merge their results in a single result.
/// </summary>
[<RequireQualifiedAccess>]
module ResultSeq

/// <summary>
/// Definition of a function that can return a Result.
/// </summary>
type ResultFunc<'Input,'Output,'TError> = 'Input -> Result<'Output,'TError>

/// <summary>
/// Executes multiple <see cref="ResultFunc" /> functions 
/// and collects the results folded in a single Result containing either the 
/// list of successful outputs (if all function calls were successful) or a list 
/// of errors (for all unsuccessful function calls). 
/// </summary>
let fold 
    (funcs: ResultFunc<'Input,'Output,'TError> seq) 
    (input: 'Input)
    : Result<'Output list, 'TError list> =
    let mutable anyErrors = false
    let mutable collectedOutputs = []
    let mutable collectedErrors = []

    let runValidator (validator: ResultFunc<'Input,'Output,'TError>) input =
        let validatorResult = validator input
        match validatorResult with
        | Error error -> 
            anyErrors <- true
            collectedErrors <- error :: collectedErrors
        | Ok output ->
            collectedOutputs <- output :: collectedOutputs

    funcs 
    |> Seq.iter(fun validator -> runValidator validator input)

    match anyErrors with
    | true -> Error collectedErrors
    | false -> Ok collectedOutputs
