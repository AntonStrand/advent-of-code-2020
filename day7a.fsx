(*
    Find "valid" passports.
    A convoluted way of solving a simple problem.
    Using Parser combinators to verify each passport category.
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

let noBags = pstring "no other bags." >>. pstring ""

let exisitingBag =
    amount
    >>. bag
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
    |>> fun ((bag, _), containg) -> (bag, (List.filter (System.String.IsNullOrEmpty >> not) containg))

let addBag bag rules key =
    match Map.tryFind key rules with
    | Some bags -> Map.add key (Set.add bag bags) rules
    | None -> Map.add key (Set.ofList [ bag ]) rules

let parse =
    many parseRow
    |>> List.fold (fun rules (bag, bags) -> bags |> List.fold (addBag bag) rules) Map.empty

let findAllPossibleBags rules =
    let rec inner bags (bag: string) =
        match Map.tryFind bag rules with
        | Some bs -> Set.fold (fun acc b -> inner (Set.add b acc) b) bags bs
        | None -> bags

    inner Set.empty "shiny gold"

let solve input =
    match run parse input with
    | Success (rules, _) -> findAllPossibleBags rules |> Set.count
    | Failure _ -> 0

solve input |> printfn "Solution %A"
