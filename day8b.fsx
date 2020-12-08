(*
    Run your copy of the boot code. Immediately before any instruction is executed a second time, what value is in the accumulator?
*)

#load "Parser.fsx"
open System.IO
open Parser

let input = File.ReadAllText "./day8-input.txt"

type Operation =
    | Acc of int
    | Jmp of int
    | Noop of int


let pint =
    let zero = pstring "0"

    let noneZeroInt =
        anyOf [ '1' .. '9' ]
        .>>. many (anyOf [ '0' .. '9' ])
        |>> fun (digit, digits) -> digit :: digits
        |>> charListToString

    (pchar '-' <|> pchar '+')
    .>>. (zero <|> noneZeroInt)
    |>> fun (sign, number) -> if sign = '-' then "-" + number else number
    |>> int

let pacc = pstring "acc " >>. pint |>> Acc
let pjmp = pstring "jmp " >>. pint |>> Jmp
let pnop = pstring "nop " >>. pint |>> Noop

let pcommand = choice [ pacc; pjmp; pnop ]

let pcommands = many1 (pcommand .>> opt (pchar '\n'))

let updateCmd cmd at =
    Array.mapi (fun i c -> if i = at then cmd else c)

type State = int * int * Set<int>

type ExecutionResult =
    | Executed of State
    | Done of int
    | Looping

let execute commands (acc, index, executed) =
    match Array.tryItem index commands with
    | Some command ->
        if Set.contains index executed then
            Looping
        else
            let execs = Set.add index executed
            Executed
            <| match command with
               | Acc n -> (acc + n, index + 1, execs)
               | Jmp dir -> (acc, index + dir, execs)
               | Noop _ -> (acc, index + 1, execs)
    | None -> Done acc

let makeBruteforceData commands =
    let rec inner variations i =
        match Array.tryItem i commands with
        | Some command ->
            match command with
            | Acc _ -> inner variations (i + 1)
            | Jmp dir -> inner (updateCmd (Noop dir) i commands :: variations) (i + 1)
            | Noop dir -> inner (updateCmd (Jmp dir) i commands :: variations) (i + 1)
        | None -> variations

    inner [ Array.empty ] 0

let runAllCommands bruteforceData =
    let startState = (0, 0, Set.empty)

    let rec inner state variations =
        match variations with
        | [] -> 0
        | cmds :: remaining ->
            match execute cmds state with
            | Executed next -> inner next variations
            | Looping -> inner startState remaining
            | Done acc -> acc

    inner startState bruteforceData

let solve input =
    match run pcommands input with
    | Success (commands, _) ->
        commands
        |> Array.ofList
        |> makeBruteforceData
        |> runAllCommands
    | Failure _ -> 0

solve input |> printfn "Solution %A"
