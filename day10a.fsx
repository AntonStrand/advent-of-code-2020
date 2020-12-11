(*
    Encoding error
*)

#load "Parser.fsx"
open System.IO
open Parser

let input = File.ReadAllText "./day10-input.txt"

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
    |>> int

let parse =
    many1 (pint .>> opt lineBreak) |>> List.sort

let getSteps adapters =
    let rec inner (s1, s3, numbers) =
        match numbers with
        | x :: y :: nums ->
            if y - x = 1 then inner (s1 + 1, s3, y :: nums)
            else if y - x = 3 then inner (s1, s3 + 1, y :: nums)
            else inner (s1, s3, nums)
        | _ -> s1 * s3

    inner (0, 0, 0 :: adapters @ [ List.last adapters + 3 ])

let solve input =
    match run parse input with
    | Success (numbers, _) -> getSteps numbers
    | Failure _ -> 0

solve input |> printfn "Solution %A"
