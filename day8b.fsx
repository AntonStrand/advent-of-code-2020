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

let always x _ = x

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
let pnop = pstring "nop " >>. pint |>> always Noop

let pcommand = choice [ pacc; pjmp; pnop ]

let pcommands = many1 (pcommand .>> opt (pchar '\n'))

let updateCommandAt at cmd cmds =
    cmds
    |> Array.mapi (fun i c -> if i = at then cmd else c)

// let findRepitition commands =
//     let numCommands = List.length commands - 1

//     let rec inner visited acc previous idx cmds =
//         printfn "%A  | %A" (Array.get cmds idx) (idx + 1)
//         if Set.count visited = numCommands then
//             (List.ofArray cmds, acc)
//         else if not <| Set.contains idx visited then
//             let v = Set.add idx visited
//             match Array.get cmds idx with
//             | Acc n -> inner v (acc + n) idx (idx + 1) cmds
//             | Jmp dir -> inner v acc idx (idx + dir) cmds
//             | Noop _ -> inner v acc idx (idx + 1) cmds
//         else
//             match Array.get cmds previous with
//             | Acc n -> inner visited (acc + n) idx (previous + 1) cmds
//             | Jmp dir -> inner visited acc idx (previous + 1) (updateCommandAt idx (Noop dir) cmds)
//             | Noop dir -> inner visited acc idx (previous + dir) (updateCommandAt idx (Jmp dir) cmds)

//     inner Set.empty 0 0 0 (Array.ofList commands)

let isErrorCmd idx prev commands =
    let rec inner i cmds =
        if i < Array.length cmds then
            match Array.item i cmds with
            | Jmp dir -> idx + i + dir > prev
            | _ -> inner (i + 1) cmds
        else
            false

    inner 0 (Array.sub commands idx (prev - idx))

let findRepitition commands =
    let numCommands = Array.length commands

    let rec inner acc prev idx =
        printfn "%A %A %A %A" idx prev (idx < prev) (isErrorCmd idx prev commands)
        if idx < prev then
            match isErrorCmd idx prev commands with
            | true -> inner acc idx (prev + 1)
            | false -> inner acc idx idx
        else if idx < numCommands then
            match Array.get commands idx with
            | Acc n -> inner (acc + n) idx (idx + 1)
            | Jmp dir -> inner acc idx (idx + dir)
            | Noop -> inner acc idx (idx + 1)
        else
            acc

    inner 0 -1 0

let solve input =
    match run pcommands input with
    | Success (commands, _) -> commands |> Array.ofList |> findRepitition
    // |> fst
    // |> findRepitition
    // |> snd
    | Failure _ -> 0

solve input |> printfn "Solution %A"
