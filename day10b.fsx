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

let withOutlet adapters = 0 :: adapters

let chunkAdjacentCount =
    List.fold (fun acc x ->
        match List.tryHead acc with
        | Some (prev, sum) when prev + 1 = x -> (x, sum + 1) :: List.tail acc
        | Some _ -> (x, 1) :: acc
        | None -> (0, 1) :: acc) []
    >> List.rev
    >> List.map snd

// https://brilliant.org/wiki/tribonacci-sequence/
let tribonacciSequence = [| 1; 1; 2; 4; 7; 13; 24; 44; 81; 149 |]
let getTribonacci num = Array.item (num - 1) tribonacciSequence

let solve input =
    match run parse input with
    | Success (adapters, _) ->
        adapters
        |> withOutlet
        |> chunkAdjacentCount
        |> List.map (getTribonacci >> int64)
        |> List.fold (*) 1L
    | Failure _ -> 0L

solve input |> printfn "Solution %d"
