/// Contains math functions that use a tolerance parameter to try to avoid
/// floating-point rounding errors.
module TolerantMath

/// Determines whether the value is near zero.
let isZero (tolerance: float) (value: float) = abs value <= tolerance

/// Possible results of the compareTo function.
type ValuesComparisonResult =
    | LessThan
    | Equal
    | GreaterThan

/// Compares two float values, taking into account the comparison tolerance.
let compareTo tolerance value1 value2 =
    if value2 < value1 - tolerance then LessThan
    elif value2 < value1 + tolerance then Equal
    else GreaterThan

/// Possible results of the determineValue01Status function.
type Value01Determinator =
    | Is0Or1
    | Between0And1
    | Outside

/// Given a value and a tolerance, determines whether it is a value below 0,
/// exactly 0, between 0 and 1, exactly 1 or above 1.
let determineValue01Status tolerance value =
    match
        (value |> compareTo tolerance 0.), (value |> compareTo tolerance 1.)
    with
    | Equal, LessThan -> Is0Or1
    | GreaterThan, Equal -> Is0Or1
    | LessThan, LessThan -> Outside
    | GreaterThan, GreaterThan -> Outside
    | GreaterThan, LessThan -> Between0And1
    | _ -> invalidOp "bug: this should never happen"
