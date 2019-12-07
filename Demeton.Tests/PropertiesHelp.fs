/// <summary>
/// Contains common reusable FsCheck properties and functions.
/// </summary>
module PropertiesHelp

open FsCheck
open BetterFsCheckRunner
    
let floatInRange (minValue: int) (maxValue: int) =
    let scaleFactor = 100

    Gen.choose(minValue * scaleFactor, (maxValue - 1) * scaleFactor) 
    |> Gen.map (fun i -> float i / float scaleFactor)

let floatInRangeInclusive (minValue: int) (maxValue: int) =
    let scaleFactor = 100

    Gen.choose(minValue * scaleFactor, maxValue * scaleFactor) 
    |> Gen.map (fun i -> float i / float scaleFactor)
    
let floatFrom0To1Inclusive granularity =
    Gen.choose(0, granularity) 
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

let (.=.) left right = left = right |@ sprintf "%A = %A" left right

let private optionsWithXUnitRunner output verbose =
    { Config.QuickThrowOnFailure with
        QuietOnSuccess = false
        Runner = betterXUnitRunner output verbose        
    }    

let private optionsWithVerboseXUnitOutput
    (output: Xunit.Abstractions.ITestOutputHelper) =
    // stolen from https://github.com/fscheck/FsCheck/blob/master/src/FsCheck.Xunit/PropertyAttribute.fs
    { optionsWithXUnitRunner output true with
        QuietOnSuccess = false
        Every = Config.Verbose.Every
        EveryShrink = Config.Verbose.EveryShrink
    }

/// Runs FsCheck on the specified property, using the specified generator.
/// Does not output debug information if the property check is successful.
let checkProperty generator output property =
    let property' =
        generator 
        |> Arb.fromGen
        |> Prop.forAll <| property
    
    Check.One((optionsWithXUnitRunner output false), property')
    
/// Runs FsCheck on the specified property, using the specified generator
/// and the provided max tests and end size values.
/// Does not output debug information if the property check is successful.
let checkPropertyWithTestSize generator output maxTests endSize property =
    let property' =
        generator 
        |> Arb.fromGen
        |> Prop.forAll <| property
        
    Check.One(
        { (optionsWithXUnitRunner output false) 
            with MaxTest = maxTests; EndSize = endSize },
        property'
    )
    
/// Runs FsCheck on the specified property, using the specified generator.
/// Outputs verbose debug information even if the property check is successful,
/// so it needs Xunit's ITestOutputHelper.
let checkPropertyVerbose
    generator (output: Xunit.Abstractions.ITestOutputHelper) property =
    let property' =
        generator 
        |> Arb.fromGen
        |> Prop.forAll <| property
        
    Check.One(
        { (optionsWithVerboseXUnitOutput output) with 
            QuietOnSuccess = false
        },
        property')

/// Runs FsCheck on the specified property, using the specified generator
/// and the provided max tests and end size values.
/// Outputs verbose debug information even if the property check is successful,
/// so it needs Xunit's ITestOutputHelper.
let checkPropertyVerboseWithTestSize
    generator output maxTests endSize property =
    let property' =
        generator 
        |> Arb.fromGen
        |> Prop.forAll <| property
        
    Check.One(
        { (optionsWithVerboseXUnitOutput output) with 
            QuietOnSuccess = false; MaxTest = maxTests; EndSize = endSize
        },
        property')
    
/// Replays the FsCheck property check using the specified replay seed.
/// Outputs verbose debug information even if the property check is successful,
/// so it needs Xunit's ITestOutputHelper.
let replayPropertyCheck generator output replaySeed property =
    let property' =
        generator 
        |> Arb.fromGen
        |> Prop.forAll <| property
    
    Check.One(
        { (optionsWithVerboseXUnitOutput output) with 
            Replay = Some <| Random.StdGen replaySeed; 
            QuietOnSuccess = false },
        property')
