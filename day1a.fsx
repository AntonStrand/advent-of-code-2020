#load "day1-input.fsx"
open Input

let rec solve entries =
    match entries with
    | head :: tail ->
        tail
        |> List.tryFind ((+) head >> (=) 2020)
        |> Option.map ((*) head)
        |> Option.defaultValue (solve tail)
    | [] -> 0

solve input |> printfn "Solution: %A"
