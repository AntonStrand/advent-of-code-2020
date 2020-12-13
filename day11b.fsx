(*
    Find occupied seats
*)

#load "Parser.fsx"
open System.IO
open Parser

let input = File.ReadAllText "./day11-input.txt"

let always x _ = x

let ifElse pred t f x = if pred x then t else f

type Area =
    | Floor
    | EmptySeat
    | OccupiedSeat

let pfloor = pchar '.' |>> always Floor
let pseat = pchar 'L' |>> always EmptySeat

let prow =
    many1 (choice [ pfloor; pseat ]) |>> Array.ofList

let parse =
    many1 (prow .>> opt (pchar '\n')) |>> Array.ofList

let visibleSeatAt (y, x) pos map =
    let rec inner (row, col) =
        Array.tryItem row map
        |> Option.bind (Array.tryItem col)
        |> Option.bind (function
            | OccupiedSeat -> Some OccupiedSeat
            | EmptySeat -> Some EmptySeat
            | Floor -> inner (row + y, col + x))

    inner (fst pos + y, snd pos + x)
    |> Option.defaultValue Floor

let visibileSeats pos map =
    [ visibleSeatAt (-1, 0) pos map
      visibleSeatAt (-1, 1) pos map
      visibleSeatAt (0, 1) pos map
      visibleSeatAt (1, 1) pos map
      visibleSeatAt (1, 0) pos map
      visibleSeatAt (1, -1) pos map
      visibleSeatAt (0, -1) pos map
      visibleSeatAt (-1, -1) pos map ]

let handleEmptySeat pos map =
    visibileSeats pos map
    |> ifElse (List.exists ((=) OccupiedSeat)) EmptySeat OccupiedSeat

let handleOccupiedSeat pos map =
    visibileSeats pos map
    |> List.filter ((=) OccupiedSeat)
    |> ifElse (List.length >> (<=) 5) EmptySeat OccupiedSeat

let getArea map y =
    Array.mapi (fun x area ->
        match area with
        | Floor -> Floor
        | EmptySeat -> handleEmptySeat (y, x) map
        | OccupiedSeat -> handleOccupiedSeat (y, x) map)

let rec simulate map =
    let newMap = map |> Array.mapi (getArea map)
    if (Array.exists2 (<>) map newMap) then simulate newMap else newMap

let calcOccupied =
    Array.concat
    >> Array.filter ((=) OccupiedSeat)
    >> Array.length

let solve input =
    match run parse input with
    | Success (map, _) -> map |> simulate |> calcOccupied
    | Failure _ -> 0

solve input |> printfn "Solution %d"
