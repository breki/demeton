/// <summary>
/// Contains common reusable FsCheck properties and functions.
/// </summary>
module PropertiesHelp

open FsCheck

let floatInRangeInclusive (minValue: int) (maxValue: int) =
    let scaleFactor = 100

    Gen.choose(minValue * scaleFactor, (maxValue + 1) * scaleFactor) 
    |> Gen.map (fun i -> float i / float scaleFactor)
    
let floatInRangeExclusive (minValue: int) (maxValue: int) =
    let scaleFactor = 100

    Gen.choose(minValue * scaleFactor, maxValue * scaleFactor) 
    |> Gen.map (fun i -> float i / float scaleFactor)
    
let floatFrom0To1Inclusive granularity =
    Gen.choose(0, granularity + 1) 
    |> Gen.map (fun i -> float i / float granularity)

let optionOfWithFrequency (frequency: int) g = 
    Gen.frequency [
        (frequency, gen { return None }); 
        (100-frequency, Gen.map Some g)]

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

let (.=.) left right = left = right |@ sprintf "%A = %A" left right