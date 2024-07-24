[<RequireQualifiedAccess>]
module Should

open System

let notBeCalled _ = invalidOp "bug: should not be called"
let notBeCalled2 _ _ = invalidOp "bug: should not be called"
let notBeCalled3 _ _ _ = invalidOp "bug: should not be called"
let notBeCalled4 _ _ _ _ = invalidOp "bug: should not be called"

let fail message = invalidOp message

let reportNotImplemented message =
    NotImplementedException message |> raise
