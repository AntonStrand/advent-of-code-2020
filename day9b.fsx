(*
    Encoding error
*)

#load "Parser.fsx"
open System.IO
open Parser

let input = File.ReadAllText "./day9-input.txt"

let lineBreak = pchar '\n'

let pint =
    let zero = pstring "0"

    let noneZeroInt =
        anyOf [ '1' .. '9' ]
        .>>. many (anyOf [ '0' .. '9' ])
        |>> fun (digit, digits) -> digit :: digits
        |>> charListToString

    opt (pchar '-')
    .>>. (zero <|> noneZeroInt)
    |>> fun (sign, number) -> if Option.isSome sign then "-" + number else number
    |>> int64

let parse = many1 (pint .>> opt lineBreak)

let isEncryptionList (goal: int64) numbers =
    let rec inner (sum, list) nums =
        match nums with
        | h :: tail when sum + h < goal -> inner (sum + h, h :: list) tail
        | h :: _ when sum + h = goal -> Some(h :: list)
        | _ -> None

    inner (0L, []) numbers

let decodeEncryption list = List.min list + List.max list

let decodeXMAS n numbers =
    let rec inner nums =
        if List.isEmpty nums then
            []
        else
            match isEncryptionList n nums with
            | Some list -> list
            | None -> inner (List.tail nums)

    decodeEncryption (inner numbers)

let solve input =
    match run parse input with
    | Success (numbers, _) -> decodeXMAS 57195069L numbers
    | Failure _ -> 0L

solve input |> printfn "Solution %d"
