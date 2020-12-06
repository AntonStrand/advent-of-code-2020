(*
    Find "valid" passports.
    A convoluted way of solving a simple problem.
    Using Parser combinators to verify each passport category.
*)

#load "Parser.fsx"
open System.IO
open Parser

let input = File.ReadAllText "./day4-input.txt"

let always x _ = x

let whitespace = many (anyOf [ ' '; '\n'; '\t' ])

let digit = anyOf [ '0' .. '9' ]

let byr =
    pstring "byr"
    .>> pchar ':'
    .>> choice [ (pstring "19"
                  .>>. sequence [ anyOf [ '2' .. '9' ]; digit ])
                 (pstring "200"
                  .>>. (anyOf [ '0' .. '2' ] |>> List.singleton)) ]
    .>> whitespace

let iyr =
    pstring "iyr"
    .>> pstring ":20"
    .>> choice [ sequence [ pchar '1'; digit ] |>> charListToString
                 pstring "20" ]
    .>> whitespace

let eyr =
    pstring "eyr"
    .>> pstring ":20"
    .>> choice [ sequence [ pchar '2'; digit ] |>> charListToString
                 pstring "30" ]
    .>> whitespace

let cm =
    pchar '1'
    >>. (sequence [ anyOf [ '5' .. '8' ]; digit ]
         <|> sequence [ pchar '9'
                        anyOf [ '0' .. '3' ] ])
    >>. pstring "cm"

let inch =
    choice [ sequence [ pchar '5'; pchar '9' ]
             sequence [ pchar '6'; digit ]
             sequence [ pchar '7'
                        anyOf [ '0' .. '6' ] ] ]
    >>. pstring "in"

let hgt =
    pstring "hgt"
    .>> pchar ':'
    .>> choice [ cm; inch ]
    .>> whitespace


let hex = anyOf ([ '0' .. '9' ] @ [ 'a' .. 'f' ])

let hcl =
    pstring "hcl"
    .>> sequence (pchar ':' :: pchar '#' :: List.replicate 6 hex)
    .>> whitespace

let ecl =
    pstring "ecl"
    .>> pchar ':'
    .>> ([ "amb"
           "blu"
           "brn"
           "gry"
           "grn"
           "hzl"
           "oth" ]
         |> List.map pstring
         |> choice)
    .>> whitespace

let pid =
    pstring "pid"
    .>> pchar ':'
    .>> sequence (List.replicate 9 digit)
    .>> whitespace

let cid =
    pstring "cid"
    .>> pchar ':'
    .>> many1 digit
    .>> whitespace

let toPassports (str: string) = str.Split "\n\n" |> List.ofArray

let parsePassport =
    choice [ byr
             iyr
             eyr
             hgt
             hcl
             ecl
             pid
             cid ]
    |> many
    |> noRemainingInput
    |>> Set.ofList

let requiredCategories =
    Set.ofList [ "byr"
                 "iyr"
                 "eyr"
                 "hgt"
                 "hcl"
                 "ecl"
                 "pid" ]

let isValidPassport passport =
    match run parsePassport passport with
    | Success (parsed, _) -> Set.isSubset requiredCategories parsed
    | Failure _ -> false

let solve =
    toPassports
    >> List.where isValidPassport
    >> List.length

solve input |> printfn "Solution %A"
