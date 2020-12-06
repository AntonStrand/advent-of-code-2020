(*
    Find "valid" passports.
    A convoluted way of solving a simple problem.
    Using Parser combinators to verify each passport category.
*)

#load "Parser.fsx"
open System.IO
open Parser

let input = File.ReadAllText "./day4-input.txt"

let whitespace = many (anyOf [ ' '; '\n'; '\t' ])

let digit = anyOf [ '0' .. '9' ]

let byr19 =
    sequence [ pchar '1'
               pchar '9'
               anyOf [ '2' .. '9' ]
               digit ]

let byr20 =
    sequence [ pchar '2'
               pchar '0'
               pchar '0'
               anyOf [ '0' .. '2' ] ]

let pByr =
    choice [ byr19; byr20 ]
    .>> whitespace
    |> noRemainingInput
    |>> charListToString

let iyr10 =
    sequence [ pchar '1'; digit ] |>> charListToString

let iyr20 = pstring "20"

let pIyr =
    sequence [ iyr20
               choice [ iyr10; iyr20 ] ]
    .>> whitespace
    |> noRemainingInput
    |>> List.reduce (+)

let eyr20 =
    sequence [ pchar '2'; digit ] |>> charListToString

let eyr30 = pstring "30"

let pEyr =
    pstring "20"
    .>>. choice [ eyr20; eyr30 ]
    .>> whitespace
    |>> fun (h, d) -> h + d

let cm =
    pchar '1'
    .>>. (sequence [ anyOf [ '5' .. '8' ]; digit ]
          <|> sequence [ pchar '9'
                         anyOf [ '0' .. '3' ] ])
    .>>. pstring "cm"
    |>> fun ((h, ds), cm) -> charListToString (h :: ds) + cm

let inch =
    choice [ sequence [ pchar '5'; pchar '9' ]
             sequence [ pchar '6'; digit ]
             sequence [ pchar '7'
                        anyOf [ '0' .. '6' ] ] ]
    .>>. pstring "in"
    |>> fun (ds, inch) -> charListToString ds + inch

let pHgt =
    choice [ cm; inch ]
    .>> whitespace
    |> noRemainingInput

let hex = anyOf ([ '0' .. '9' ] @ [ 'a' .. 'f' ])

let pHcl =
    sequence (pchar '#' :: List.replicate 6 hex)
    .>> whitespace
    |> noRemainingInput
    |>> charListToString

let pEcl =
    [ "amb"
      "blu"
      "brn"
      "gry"
      "grn"
      "hzl"
      "oth" ]
    |> List.map pstring
    |> choice
    |> noRemainingInput


let pPid =
    sequence (List.replicate 9 digit)
    .>> whitespace
    |> noRemainingInput
    |>> charListToString

let categoryValidation =
    [ ("byr", pByr)
      ("iyr", pIyr)
      ("eyr", pEyr)
      ("hgt", pHgt)
      ("hcl", pHcl)
      ("ecl", pEcl)
      ("pid", pPid) ]

let split delimiter (string: string) = string.Split delimiter

let pickCategories =
    split [| ' '; '\n' |]
    >> Array.map
        (split [| ':' |]
         >> (fun category -> (Array.head category, Array.last category)))
    >> Map.ofArray

let toPassports (str: string) =
    str.Split "\n\n"
    |> List.ofArray
    |> List.map pickCategories

let traverse f list =
    list
    |> List.fold (fun mAcc x ->
        mAcc
        |> Option.bind (fun acc ->
            match f x with
            | Some y -> Some(y :: acc)
            | None -> None)) (Some [])

let isValidPassport passport =
    categoryValidation
    |> traverse (fun (key, parser) ->
        Map.tryFind key passport
        |> Option.bind (fun category ->
            match run parser category with
            | Success v -> Some v
            | Failure _ -> None))

let solve =
    toPassports
    >> List.choose isValidPassport
    >> List.length

solve input |> printfn "Solution %A"
