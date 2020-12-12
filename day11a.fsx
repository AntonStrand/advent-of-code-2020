(*
    Find occupied seats
*)

#load "Parser.fsx"
open System.IO
open Parser

let input = File.ReadAllText "./day11-input.txt"

let always x _ = x

type Area =
    | Floor
    | EmptySeat
    | OccupiedSeat

let pfloor = pchar '.' |>> always Floor
let pseat = pchar 'L' |>> always EmptySeat

let parse =
    many1 (choice [ pfloor; pseat ] .>> opt (pchar '\n'))
    |>> Array.ofList

let adjacentOffset pos =
    (pos - 91, pos - 90, pos - 89, pos - 1, pos + 1, pos + 89, pos + 90, pos + 91)

let getAdjacentSeats pos map =
    let (fl, fm, fr, l, r, bl, bm, br) = adjacentOffset pos
    match pos % 90 with
    | 0 -> [ fm; fr; r; bm; br ]
    | 89 -> [ fl; fm; l; bl; bm ]
    | _ -> [ fl; fm; fr; l; r; bl; bm; br ]
    |> List.filter (fun x -> x >= 0 && x < Array.length map)

let handleEmptySeat pos map =
    getAdjacentSeats pos map
    |> List.exists (fun seat -> Array.item seat map = OccupiedSeat)
    |> function
    | true -> EmptySeat
    | false -> OccupiedSeat

let handleOccupiedSeat pos map =
    getAdjacentSeats pos map
    |> List.filter (fun seat -> Array.item seat map = OccupiedSeat)
    |> (List.length >> (<=) 4)
    |> function
    | true -> EmptySeat
    | false -> OccupiedSeat

let getArea map pos =
    match Array.item pos map with
    | Floor -> Floor
    | EmptySeat -> handleEmptySeat pos map
    | OccupiedSeat -> handleOccupiedSeat pos map

let rec simulate map =
    let newMap =
        [| 0 .. (Array.length map - 1) |]
        |> Array.map (getArea map)

    if (Array.exists2 (<>) map newMap) then simulate newMap else newMap

let calcOccupied =
    Array.filter ((=) OccupiedSeat) >> Array.length

let solve input =
    match run parse input with
    | Success (map, _) -> map |> simulate |> calcOccupied
    | Failure _ -> 0

solve input |> printfn "Solution %d"
