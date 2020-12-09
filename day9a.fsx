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

let rec isValid preamble (n: int64) =
    match preamble with
    | head :: tail -> if tail |> List.exists ((+) head >> (=) n) then true else isValid tail n
    | [] -> false

let decodeXMAS n numbers =
    let rec inner (preamble, remaining) =
        match remaining with
        | head :: tail ->
            if isValid preamble head
            then inner (List.tail preamble @ [ head ], tail)
            else head
        | [] -> 0L

    inner (List.splitAt n numbers)

let solve input =
    match run parse input with
    | Success (numbers, _) -> decodeXMAS 25 numbers
    | Failure _ -> 0L

solve input |> printfn "Solution %d"
