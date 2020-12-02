#load "day1-input.fsx"
open Input

let rec solve entries =
    match entries with
    | head :: tail ->
        tail
        |> List.tryPick (fun x ->
            List.tryPick (fun y -> if head + y + x = 2020 then Some(head * y * x) else None) entries)
        |> Option.defaultValue (solve tail)
    | [] -> 0

solve input |> printfn "Solution: %A"
