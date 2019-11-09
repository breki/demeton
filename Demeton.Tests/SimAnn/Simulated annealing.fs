module Tests.SimmAnn.``Simulated annealing``

open System

open Xunit
open Swensen.Unquote

type AnnealingSchedule = int -> float
type NeighborFunc<'TState> = 'TState -> Random -> 'TState
type EnergyFunc<'TState> = 'TState -> float
type AcceptanceProbabilityFunc = float -> float -> float -> float

// https://en.wikipedia.org/wiki/Simulated_annealing
// https://www.mathworks.com/help/gads/how-simulated-annealing-works.html

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
      
        let acceptNewState =
            match newStateEnergy < currentStateEnergy with
            | true -> true
            | false ->
                let acceptanceProbabilityCalculated =
                    acceptanceProbability 
                        temperature currentStateEnergy newStateEnergy
                
                rnd.NextDouble() <= acceptanceProbabilityCalculated

        match acceptNewState with
        | true -> newState
        | false -> currentState

    [| 0 .. (maxSteps-1) |]
    |> Array.fold runStep initialState

let annealingScheduleExponential 
    initialTemperature coolingFactor: AnnealingSchedule = 
    fun step -> 
    initialTemperature * (pown coolingFactor step)

let kirkpatrickAcceptanceProbability: AcceptanceProbabilityFunc =
    fun temperature currentStateEnergyValue newStateEnergyValue ->
    Math.Exp((currentStateEnergyValue - newStateEnergyValue) / temperature)

let sampleNeighbor: NeighborFunc<float> = fun x rnd ->
    x + (rnd.NextDouble() - 0.5) / 1000.

let sampleEnergy: EnergyFunc<float> = fun x -> sin x

[<Fact(Skip="todo")>]
let ``Run simulated annealing on a sinusoide``() =
    let finalState = 
        simulatedAnnealing 
            (annealingScheduleExponential 100. 0.85) 
            0. 
            sampleNeighbor 
            sampleEnergy
            kirkpatrickAcceptanceProbability
            1000 

    printfn 
        "final state: %g, energy: %g" 
            finalState (sampleEnergy finalState)

    test <@ abs ((sampleEnergy finalState) - 1.) < 0.0001 @> 


