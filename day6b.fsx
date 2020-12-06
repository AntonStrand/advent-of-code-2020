(*
    Find "valid" passports.
    A convoluted way of solving a simple problem.
    Using Parser combinators to verify each passport category.
*)

#load "Parser.fsx"
open System.IO
open Parser

let input = File.ReadAllText "./day6-input.txt"

let onePersonsAnswer =
    many1 (anyOf [ 'a' .. 'z' ])
    .>> opt (pchar '\n')
    |>> Set.ofList

let parser =
    many onePersonsAnswer
    |>> List.reduce (Set.intersect)

let parse group =
    match run parser group with
    | Success (answers, _) -> Set.count answers
    | Failure _ -> 0

let group (str: string) = str.Split "\n\n" |> List.ofArray

let solve = group >> List.map parse >> List.sum

solve input |> printfn "Solution %A"
