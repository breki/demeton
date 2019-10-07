/// <summary>
/// Contains common reusable FsCheck properties and functions.
/// </summary>
module PropertiesHelp

open FsCheck

let floatFrom0To1Inclusive =
    let maxIntToUse = 10000

    Gen.choose(0, maxIntToUse + 1) 
    |> Gen.map (fun i -> float i / float maxIntToUse)

/// <summary>
/// Property that asserts fromValue <= value <= toValue.
/// </summary>
let valueIsBetweenInclusive 
    (name: string) 
    (fromValue: 'T) 
    (toValue: 'T) 
    (value: 'T) =
    let min = min fromValue toValue
    let max = max fromValue toValue
    (min <= value && value <= max) 
        |@ sprintf "%s %A <= %A <= %A" name min value max

/// <summary>
/// Replays the FsCheck property check using the specified replay seed.
/// </summary>
let replayPropertyCheck replaySeed property =
    Check.One(
        { Config.Quick with Replay = Some <| Random.StdGen replaySeed },
        property)