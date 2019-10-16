#if INTERACTIVE
#else
module FSharpPatterns.Examples.ChainOfResposibility
#endif

// pattern
type Record = {
    Name : string
    Age : int
    Weight : float
    Height : float
}

let chainOfResponsibility() =
    let validAge record = 
        record.Age < 65 && record.Age > 18

    let validWeight record =
        record.Weight < 200.

    let validHeight record =
        record.Height > 120.

    let check f (record, result) =
        if not result then record, false
        else record, f(record)

    let chainOfResponsibility = check validAge >> check validWeight >> check validHeight

    let john = { Name = "John"; Age = 80; Weight = 180.; Height = 180. }
    let dan = { Name = "Dan"; Age = 20; Weight = 160.; Height = 190. }

    printfn "John's result = %b" (chainOfResponsibility (john, true) |> snd)
    printfn "Dans's result = %b" (chainOfResponsibility (dan, true) |> snd)


chainOfResponsibility()

// pipeline
let chainTemplate processFunction canContinue s =
    if canContinue s then
        processFunction s
    else s

let canContinueF _ = true
let processF x = x + 1

let chainFunction = chainTemplate processF canContinueF

let s = 1 |> chainFunction |> chainFunction

printfn "%A" s

// using partial pattern matching
[<Measure>] type cm
[<Measure>] type kg

type Person() =
    member val Height = 0.<cm> with get, set
    member val Weight = 0.<kg> with get, set

let makeCheck passingCriterion (person: #Person) =
    if passingCriterion person then None
    else Some person

let (|NotPassHeight|_|) person = 
    makeCheck (fun p -> p.Height > 170.<cm>) person

let (|NotPassWeight|_|) person = 
    makeCheck (fun p -> p.Weight < 100.<kg> && p.Weight > 50.<kg>) person

let check x = 
    match x with
    | NotPassHeight x -> printfn "this person is not tall enough"
    | NotPassWeight x -> printfn "this person is out of weight range"
    | _ -> printfn "good, this person passes"

let p = Person(Height = 180.<cm>, Weight = 75.<kg>)

check p