#if INTERACTIVE
#else
module FSharpPatterns.Examples.Command
#endif

// command pattern implementation I
type Command = { Redo: unit -> unit; Undo: unit -> unit }

let commandPatternSample() =
    
    // just for demo, mutable should be used whenever posible
    let result = ref 7

    let add n = {
        Redo = (fun _ -> result := !result + n)
        Undo = (fun _ -> result := !result - n)
    }

    let minus n = {
        Redo = (fun _ -> result := !result - n)
        Undo = (fun _ -> result := !result + n)
    }

    let cmd = add 3
    printfn "current state = %d" !result

    cmd.Redo()
    printfn "after redo: %d" !result

    cmd.Undo()
    printfn "after undo: %d\n" !result

commandPatternSample()

// command pattern implementation II
type CommandType =
    | Deposit
    | Withdraw

type TCommand =
    | Command of CommandType * int

let mutable result = 7

let deposit x = result <- result + x
let withdraw x = result <- result - x

let Do = function
    | Command (CommandType.Deposit, n) -> deposit n
    | Command (CommandType.Withdraw, n) -> withdraw n

let Undo = function
    | Command (CommandType.Deposit, n) -> withdraw n
    | Command (CommandType.Withdraw, n) -> deposit n

printfn "current balance %d" result

let depositCmd = Command (Deposit, 3)
Do depositCmd
printfn "after deposit: %d" result

Undo depositCmd
printfn "after undo: %d\n" result