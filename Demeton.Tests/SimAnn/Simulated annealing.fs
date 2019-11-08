module Tests.SimmAnn.``Simulated annealing``

open System

open Xunit
open Swensen.Unquote

type AnnealingSchedule = int -> float
type NeighborFunc<'TState> = 'TState -> Random -> 'TState
type AcceptanceProbabilityFunc = float -> float -> float -> float

// https://en.wikipedia.org/wiki/Simulated_annealing
// https://www.mathworks.com/help/gads/how-simulated-annealing-works.html

let simulatedAnnealing 
    (annealingSchedule: AnnealingSchedule)
    (initialState: 'TState)
    (neighbor: NeighborFunc<'TState>)
    (acceptanceProbability: AcceptanceProbabilityFunc)
    maxSteps
    : 'TState =
    
    let rnd = Random()

    let runStep (currentState: 'TState) step: 'TState =
        let timeFraction = (float maxSteps) / (float step + 1.)
        let temperature = annealingSchedule step
        let newState = neighbor currentState rnd
        invalidOp "todo"

    [| 0 .. (maxSteps-1) |]
    |> Array.fold runStep initialState

let annealingScheduleExponential 
    initialTemperature coolingFactor: AnnealingSchedule = 
    fun step -> 
    initialTemperature * (pown coolingFactor step)

let kirkpatrickAcceptanceProbability: AcceptanceProbabilityFunc =
    fun temperature currentStateEnergyValue newStateEnergyValue ->
    invalidOp "todo"

let sampleNeighbor: NeighborFunc<float> = fun x rnd ->
    x + (rnd.NextDouble() - 0.5) / 1000.

[<Fact(Skip="todo")>]
let ``Run simulated annealing on a sinusoide``() =
    let finalState = 
        simulatedAnnealing 
            (annealingScheduleExponential 100. 0.85) 
            0. 
            sampleNeighbor 
            kirkpatrickAcceptanceProbability
            1000 

    test <@ sin finalState - 1. < 0.0001 @> 


