(*
    Find "valid" passports.
    A convoluted way of solving a simple problem.
    Using Parser combinators to verify each passport category.
*)

#load "Parser.fsx"
open System.IO
open Parser

let input = File.ReadAllText "./day6-input.txt"

let personAnswers =
    many1 (anyOf [ 'a' .. 'z' ]) .>> opt (pchar '\n')

let groupAnswers =
    many1 personAnswers |>> List.concat |>> Set.ofList

let parser = many (groupAnswers .>> opt (pchar '\n'))

let solve input =
    match run parser input with
    | Success (answers, _) -> answers |> List.sumBy Set.count
    | Failure _ -> 0

solve input |> printfn "Solution %A"
