(*
    How many passwords are valid?

    The character should EITHER be on position 1 or 2 but not both.

    1-3 a: abcde       is valid:   position 1 contains a and position 3 does not.
    1-3 b: cdefg       is invalid: neither position 1 nor position 3 contains b.
    2-9 c: ccccccccc   is invalid: both position 2 and position 9 contain c.
*)

#load "day2-input.fsx"
open Input

let charAt i str = Seq.item (i - 1) str

let isValid (p1, p2, char, password) =
    if charAt p1 password = char then charAt p2 password <> char else charAt p2 password = char

let solve entries =
    Array.toList entries
    |> List.fold (fun sum entry -> if isValid entry then sum + 1 else sum) 0

solve input |> printfn "Solution %A"
