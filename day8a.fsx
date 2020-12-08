(*
    Run your copy of the boot code. Immediately before any instruction is executed a second time, what value is in the accumulator?
*)

#load "Parser.fsx"
open System.IO
open Parser

let input = File.ReadAllText "./day7-input.txt"

type Operation =
    | Acc of int
    | Jmp of int
    | Noop


let pint =
    let zero = pstring "0"

    let noneZeroInt =
        anyOf [ '1' .. '9' ]
        .>>. many (anyOf [ '0' .. '9' ])
        |>> fun (z, ds) -> z :: ds |> charListToString

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

let pcommand = choice [ pacc; pjmp; pnop ]

let pcommands = many1 (pcommand .>> opt (pchar '\n'))

let findRepitition commands =
    let rec inner visited acc idx =
        if not <| Set.contains idx visited then
            let v = Set.add idx visited
            match Array.get commands idx with
            | Acc n -> inner v (acc + n) (idx + 1)
            | Jmp dir -> inner v acc (idx + dir)
            | Noop -> inner v acc (idx + 1)
        else
            acc

    inner Set.empty 0 0


let solve input =
    match run pcommands input with
    | Success (rules, _) -> rules |> Array.ofList |> findRepitition
    | Failure _ -> 0

solve input |> printfn "Solution %A"
