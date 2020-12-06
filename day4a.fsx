(*
    Find "valid" passports.
*)

open System.IO

let input = File.ReadAllText "./day4-input.txt"

let requiredCategories =
    Set.ofList [ "byr"
                 "iyr"
                 "eyr"
                 "hgt"
                 "hcl"
                 "ecl"
                 "pid" ]

let split delimiter (string: string) = string.Split delimiter

let pickCategories =
    split [| ' '; '\n' |]
    >> Array.map (split [| ':' |] >> Array.head)
    >> Set.ofArray

let toPassports (str: string) =
    str.Split "\n\n"
    |> List.ofArray
    |> List.map pickCategories

let isValidPassport = Set.isSubset requiredCategories

let solve =
    toPassports
    >> List.where isValidPassport
    >> List.length

solve input |> printfn "Solution %A"
