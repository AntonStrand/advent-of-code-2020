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

let rec findTrees sum pos offset map =
    match map with
    | mapPart :: rest ->
        if hitsTree pos mapPart
        then findTrees (sum + 1) (pos + offset) offset rest
        else findTrees (sum) (pos + offset) offset rest
    | [] -> sum

let rec everyotherMapPartTrees sum pos offset map =
    match map with
    | _ :: mapPart :: rest ->
        if hitsTree pos mapPart
        then everyotherMapPartTrees (sum + 1) (pos + offset) offset rest
        else everyotherMapPartTrees (sum) (pos + offset) offset rest
    | _ -> sum

let solve entries =
    let map = List.ofArray entries |> List.tail

    findTrees 0 1 1 map
    * findTrees 0 3 3 map
    * findTrees 0 5 5 map
    * findTrees 0 7 7 map
    * everyotherMapPartTrees 0 1 1 map


solve input |> printfn "Solution %A"
