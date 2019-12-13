module Tests.SimmAnn.``Simulated annealing``

open SimAnn

open System

open Xunit
open Swensen.Unquote

let sampleNeighbor: NeighborFunc<float> = fun x rnd ->
    x + (rnd.NextDouble() - 0.5) / 10.

let sampleEnergy: EnergyFunc<float> = 
    // minus is because we are looking for the highest sin value, not lowest
    (fun x -> -sin x)

[<Fact>]
let ``Run simulated annealing on a sinusoide``() =
    let finalState = 
        simulatedAnnealing 
            (annealingScheduleExponential 100. 0.85) 
            0. 
            sampleNeighbor 
            sampleEnergy
            kirkpatrickAcceptanceProbability
            1000 

    //printfn 
    //    "final state: %g, energy: %g" 
    //        finalState (sampleEnergy finalState)

    let highestSinValue = sin (Math.PI / 2.)
    test <@ abs ((sin finalState) - highestSinValue) < 0.001 @> 
