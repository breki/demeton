module TolerantMath

let isZero (tolerance: float) (value: float) = abs (value) < tolerance

type ValuesComparisonResult = Below | Equal | Above
    
let compareTo tolerance value1 value2 =
    if value2 < value1 - tolerance then Below
    elif value2 < value1 + tolerance then Equal
    else Above
    
type Value01Determinator = Is0Or1 | Between0And1 | Outside

let determineValue01Status tolerance value =
    match (value |> compareTo tolerance 0.),
        (value |> compareTo tolerance 1.) with
    | Equal, Below -> Is0Or1
    | Above, Equal -> Is0Or1
    | Below, Below -> Outside
    | Above, Above -> Outside
    | Above, Below -> Between0And1
    | _ -> invalidOp "bug: this should never happen"
