(*
    Count trees in the way.
*)

#load "day3-input.fsx"
open Input

let getItem mapPart pos = Seq.item pos mapPart

let hitsTree pos mapPart =
    pos % String.length mapPart
    |> getItem mapPart
    |> (=) '#'

let rec findTrees sum pos map =
    match map with
    | mapPart :: rest ->
        if hitsTree pos mapPart then findTrees (sum + 1) (pos + 3) rest else findTrees (sum) (pos + 3) rest
    | [] -> sum

let solve map = map |> List.tail |> findTrees 0 3


solve input |> printfn "Solution %A"
