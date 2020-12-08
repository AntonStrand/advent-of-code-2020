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
    | Noop

type State = int * int * Set<int>

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

let pnop =
    pstring "nop "
    >>. pint
    |>> fun _ -> Noop

let pcommand =
    choice [ pacc; pjmp; pnop ] .>> opt (pchar '\n')

let pcommands = many1 pcommand

let findRepitition commands =
    let rec inner (acc, index, executed) =
        if not <| Set.contains index executed then
            let exed = Set.add index executed
            match Array.get commands index with
            | Acc n -> inner (acc + n, index + 1, exed)
            | Jmp dir -> inner (acc, index + dir, exed)
            | Noop -> inner (acc, index + 1, exed)
        else
            acc

    inner (0, 0, Set.empty)

let solve input =
    match run pcommands input with
    | Success (commands, _) -> commands |> Array.ofList |> findRepitition
    | Failure _ -> 0

solve input |> printfn "Solution %A"
