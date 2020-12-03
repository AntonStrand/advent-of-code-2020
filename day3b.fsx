(*
    Count trees in the way.
*)

#load "day3-input.fsx"
open Input

let getTerrain map pos = Seq.item pos map

let hitsTree (down, right) (pos, map) =
    pos % down = 0
    && ((pos / down) * right) % String.length map
       |> getTerrain map
       |> (=) '#'

let countTrees map movement =
    map
    |> List.indexed
    |> List.filter (hitsTree movement)
    |> List.length

let solve entries =
    [ (1, 1)
      (1, 3)
      (1, 5)
      (1, 7)
      (2, 1) ]
    |> List.map (countTrees (List.ofArray entries))
    |> List.fold (*) 1

solve input |> printfn "Solution %A"
