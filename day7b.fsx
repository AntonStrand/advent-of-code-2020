(*
    The required number of bags in a shiny bag
*)

#load "Parser.fsx"
open System.IO
open Parser

let input = File.ReadAllText "./day7-input.txt"

let bag =
    many1 (anyOf [ 'a' .. 'z' ])
    .>>. pchar ' '
    .>>. many1 (anyOf [ 'a' .. 'z' ])
    |>> fun ((f, _), s) -> f @ (' ' :: s)
    |>> charListToString

let amount =
    many1 (anyOf [ '0' .. '9' ])
    .>> opt (pchar ' ')
    |>> charListToString

let noBags =
    pstring "no other bags."
    |>> fun _ -> ("", "")

let exisitingBag =
    amount
    .>>. bag
    .>> pstring " bag"
    .>> opt (pchar 's')
    .>> anyOf [ '.'; ',' ]
    .>> opt (pchar ' ')

let bags = choice [ noBags; exisitingBag ]

let parseRow =
    bag
    .>>. pstring " bags contain "
    .>>. many1 bags
    .>> opt (pchar '\n')
    |>> fun ((bag, _), containg) ->
        (bag,
         (containg
          |> List.filter (fun (count, name) -> count <> "" && name <> "")
          |> List.map (fun (count, name) -> (int count, name))))

let repeatBags =
    List.map (fun (c, n) -> List.replicate c n)
    >> List.concat

let parse =
    many parseRow
    |>> List.fold (fun rules (bag, bags) -> Map.add bag (bags |> repeatBags) rules) Map.empty

let count rules =
    let rec inner sum names =
        match names with
        | n :: ns -> inner (sum + 1) (ns @ (Map.find n rules))
        | [] -> sum

    // -1 to remove "shiny gold" from the count
    inner -1 [ "shiny gold" ]

let solve input =
    match run parse input with
    | Success (rules, _) -> count rules
    | Failure _ -> 0

solve input |> printfn "Solution %A"
