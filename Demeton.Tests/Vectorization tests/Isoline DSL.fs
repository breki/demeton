module Tests.``Vectorization tests``.``Isoline DSL``

open Demeton.Vectorization.Isolines

open FParsec
open Xunit
open Swensen.Unquote

let pIsolineType: Parser<string, unit> = (pstring "o") <|> (pstring "l")
let pSeparator = pstring ";"

let pHorizOrVert = (pstring "h") <|> (pstring "v")

let pCoord = pint32

let pComma = pstring ","

let pStartingCoords =
    pipe4 pHorizOrVert pCoord pComma pCoord
        (fun hOrV x _ y -> (hOrV, x, y))

let pStep = (pstring "u") <|> (pstring "r") <|> (pstring "d") <|> (pstring "l")

let pSteps = many pStep

let parseHStep = function
    | "l" -> Some Left
    | "r" -> Some Right
    | _ -> None

let parseVStep = function
    | "u" -> Some Up
    | "d" -> Some Down
    | _ -> None

let constructSteps (hOrV, x, y) (stepChars: string list): IsolineStep list =
    let initialStepChar = stepChars |> List.head
    let restOfStepChars = stepChars |> List.tail

    let buildSteps startingStep: IsolineStep list =
        let (steps, lastStep) =
            restOfStepChars
            |> List.mapFold (fun currentStep stepChar ->
                let nextStep = 
                    match (parseHStep stepChar), (parseVStep stepChar) with
                    | (Some hStep, None) ->
                        buildNextStep currentStep (HDirection hStep)
                    | (None, Some vStep) -> buildNextStep currentStep (VDirection vStep)
                    | _ -> invalidOp "bug"
                (nextStep, nextStep)) startingStep
        startingStep :: steps
    
    match hOrV with
    | "h" ->        
        let initialStepDirection = 
            match parseHStep initialStepChar with
            | Some stepDirection -> stepDirection
            | None -> invalidOp "bug"
        let startingStep = HStep (OnHorizontalEdge(x, y), initialStepDirection)
        buildSteps startingStep 
    | "v" ->
        let initialStepDirection = 
            match parseVStep initialStepChar with
            | Some stepDirection -> stepDirection
            | None -> invalidOp "bug"
        let startingStep = VStep (OnVerticalEdge(x, y), initialStepDirection)
        buildSteps startingStep 
    | _ -> invalidOp "bug"

let pIsoline =
    pipe5 pIsolineType pSeparator pStartingCoords pSeparator pSteps
        (fun isoType _ coords _ steps ->
        match isoType with
        | "o" -> ClosedIsoline { Steps = constructSteps coords steps }
        | "l" -> ClippedIsoline { Steps = constructSteps coords steps }
        | _ -> invalidOp "bug"
    )

let parseIsolineDef isolineDef =
    match run pIsoline isolineDef with
    | Success(isoline, _, _) -> isoline
    | _ -> invalidOp "Invalid isoline definition"

[<Fact>]
let ``Can parse closed isoline definition``() =
    let isoline = parseIsolineDef "o;v0,1;drul"
                      
    test <@ isoline = ClosedIsoline {
                Steps = [
                    VStep (OnVerticalEdge (0, 1), Down)
                    HStep (OnHorizontalEdge (1, 1), Right)
                    VStep (OnVerticalEdge (1, 1), Up)
                    HStep (OnHorizontalEdge (1, 0), Left)
                ]
            } @>
    