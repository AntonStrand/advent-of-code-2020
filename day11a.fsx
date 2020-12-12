(*
    Encoding error
*)

#load "Parser.fsx"
open System.IO
open Parser

let input = File.ReadAllText "./day11-input.txt"

let lineBreak = pchar '\n'

let always x _ = x

type Area =
    | Floor
    | EmptySeat
    | OccupiedSeat

let pfloor = pchar '.' |>> always Floor
let pseat = pchar 'L' |>> always EmptySeat

let parse =
    many1 (choice [ pfloor; pseat ] .>> opt lineBreak)
    |>> Array.ofList

let rowLength = Array.length >> float >> sqrt >> int

let getAdjacentSeats (pos: int) (map: Area array): int list =
    let offset = rowLength map
    [ pos - (offset + 1) .. pos - (offset - 1) ]
    @ [ pos - 1 .. pos + 1 ]
    @ [ pos + (offset - 1) .. pos + (offset + 1) ]
    |> List.filter (fun x -> x > 0 && x < Array.length map)

let handleEmptySeat (pos: int) (map: Area array): Area =
    getAdjacentSeats pos map
    |> List.exists (fun seat -> Array.item seat map = OccupiedSeat)
    |> function
    | true -> EmptySeat
    | false -> OccupiedSeat

let handleOccupiedSeat pos map =
    getAdjacentSeats pos map
    |> List.filter (fun seat -> Array.item seat map = OccupiedSeat)
    |> (List.length >> (>=) 4)
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

    newMap

let viewMap =
    Array.map (function
        | Floor -> '.'
        | EmptySeat -> 'L'
        | OccupiedSeat -> '#')
    >> Array.chunkBySize (10)
    >> Array.collect (Array.append [| '\n' |])
    >> System.String

// if (Array.exists2 (<>) map newMap) then simulate newMap else newMap

// run (parse |>> getArea 15) """L.LL.LL.LL
// LLLLLLL.LL
// L.L.L..L..
// LLLL.LL.LL
// L.LL.LL.LL
// L.LLLLL.LL
// ..L.L.....
// LLLLLLLLLL
// L.LLLLLL.L
// L.LLLLL.LL"""



let solve input =
    match run parse input with
    | Success (map, _) -> simulate map |> simulate |> simulate |> viewMap
    | Failure _ -> "[||]"

solve input |> printfn "Solution %A"
