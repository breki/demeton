/// <summary>
/// Simulated annealing algorithm.  
/// </summary>
module SimAnn

open System

// https://en.wikipedia.org/wiki/Simulated_annealing
// https://www.mathworks.com/help/gads/how-simulated-annealing-works.html

/// <summary>
/// A function defining the schedule for the simulated annealing. Given a step
/// count, it returns the temperature.
/// </summary>
type AnnealingSchedule = int -> float

/// <summary>
/// A function that generates a neighborhood state for the provided state.
/// An instance of <see cref="System.Random" /> is provided for randomness.
/// </summary>
type NeighborFunc<'TState> = 'TState -> Random -> 'TState

/// <summary>
/// A function that calculates the energy for a given state.
/// </summary>
type EnergyFunc<'TState> = 'TState -> float

/// <summary>
/// A function that, given the temperature, current state energy and the new
/// state energy, calculates the acceptance probability.
/// </summary>
type AcceptanceProbabilityFunc = float -> float -> float -> float

/// <summary>
/// An exponential annealing schedule, defined by the initial temperature and
/// the cooling factor. It is a default annealing schedule for the simulated 
/// annealing.
/// </summary>
let inline annealingScheduleExponential 
    initialTemperature (coolingFactor: float): AnnealingSchedule = 
    fun step -> 
    initialTemperature * (pown coolingFactor step)

/// <summary>
/// Kirkpatrick's acceptance probability, which is the default acceptance 
/// probability function for the simulated annealing.
/// </summary>
let inline kirkpatrickAcceptanceProbabilityInline 
    temperature currentStateEnergy newStateEnergy =
    Math.Exp((currentStateEnergy - newStateEnergy) / temperature)

/// <summary>
/// Kirkpatrick's acceptance probability, which is the default acceptance 
/// probability function for the simulated annealing.
/// </summary>
let kirkpatrickAcceptanceProbability: AcceptanceProbabilityFunc =
    kirkpatrickAcceptanceProbabilityInline

/// <summary>
/// Runs simulated annealing, given all the provided parameters, and returns
/// the final state.
/// </summary>
let simulatedAnnealing 
    (annealingSchedule: AnnealingSchedule)
    (initialState: 'TState)
    (neighbor: NeighborFunc<'TState>)
    (energy: EnergyFunc<'TState>)
    (acceptanceProbability: AcceptanceProbabilityFunc)
    maxSteps
    : 'TState =
    
    let rnd = Random()

    let runStep (currentState: 'TState) step: 'TState =
        let temperature = annealingSchedule step
        let newState = neighbor currentState rnd

        let currentStateEnergy = energy currentState
        let newStateEnergy = energy newState
      
        match newStateEnergy < currentStateEnergy with
        | true -> 
            //printfn "S': %A, E': %g" newState newStateEnergy
            newState
        | false ->
            let acceptanceProbabilityCalculated =
                acceptanceProbability 
                    temperature currentStateEnergy newStateEnergy
               
            if rnd.NextDouble() <= acceptanceProbabilityCalculated then
                //printfn 
                //    "S': %A, E': %g, aP: %g"
                //        newState newStateEnergy acceptanceProbabilityCalculated
                newState
            else
                currentState

    [| 0 .. (maxSteps-1) |]
    |> Array.fold runStep initialState
