(*
    Find all valid passwords.
*)

#load "day2-input.fsx"
open Input

let isWithinRange min max num = num >= min && num <= max

let isValid (min, max, char, password) =
    password
    |> Seq.toList
    |> List.fold (fun occurencies letter -> if char = letter then occurencies + 1 else occurencies) 0
    |> isWithinRange min max

let solve entries =
    Array.toList entries
    |> List.fold (fun sum entry -> if isValid entry then sum + 1 else sum) 0

solve input |> printfn "Solution %A"
