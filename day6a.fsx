(*
    Find "valid" passports.
    A convoluted way of solving a simple problem.
    Using Parser combinators to verify each passport category.
*)

#load "Parser.fsx"
open System.IO
open Parser

let input = File.ReadAllText "./day6-input.txt"

let parser =
    many (anyOf [ 'a' .. 'z' ] .>> opt (pchar '\n'))
    |>> Set.ofList
    |>> Set.count

let parse group =
    match run parser group with
    | Success (count, _) -> count
    | Failure _ -> 0

let group (str: string) = str.Split "\n\n" |> List.ofArray

let solve = group >> List.map parse >> List.sum

solve input |> printfn "Solution %A"
