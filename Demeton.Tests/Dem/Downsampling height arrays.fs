module Tests.Dem.``Downsampling height arrays``
open Demeton.DemTypes

let downsampleRow
    (source: HeightsArray) (dest: HeightsArray)
    (reductionFactor: int)
    (destY: int)
    :unit =
        
    let srcTop = destY * reductionFactor   
    let srcBottom = (destY + 1) * reductionFactor
    
    for destX in 0 .. (dest.Width - 1) do
        let srcLeft = destX * reductionFactor
        let srcRight = (destX + 1) * reductionFactor
        
        let heightSum = 0
    
        // left fractional edge
        for srcY in srcTop + 1 .. (srcBottom - 1) do
            invalidOp "todo"
//            let height = source.heightAt (srcLeft, srcY)
