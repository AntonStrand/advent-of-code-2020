(*
    Encoding error
*)

#load "Parser.fsx"
open System.IO
open Parser

let input = File.ReadAllText "./day11-input.txt"

let lineBreak = pchar '\n'

let always x _ = x

let debug x =
    printfn "%A" x
    x

type Area =
    | Floor
    | EmptySeat
    | OccupiedSeat

let pfloor = pchar '.' |>> always Floor
let pseat = pchar 'L' |>> always EmptySeat

let parse =
    many1 (choice [ pfloor; pseat ] .>> opt lineBreak)
    |>> Array.ofList

let rowLength map =
    map |> Array.length |> float |> sqrt |> int

let getAdjacentSeats pos map =
    let offset = 90
    let fl = pos - (offset + 1)
    let fm = pos - offset
    let fr = pos - (offset - 1)
    let l = pos - 1
    let r = pos + 1
    let bl = pos + (offset - 1)
    let bm = pos + offset
    let br = pos + (offset + 1)
    match pos % offset with
    | 0 -> [ fm; fr; r; bm; br ]
    | 89 -> [ fl; fm; l; bl; bm ]
    | _ -> [ fl; fm; fr; l; r; bl; bm; br ]
    |> List.filter (fun x -> x >= 0 && x < Array.length map)


let handleEmptySeat (pos: int) (map: Area array): Area =
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


let viewMap =
    Array.map (function
        | Floor -> '.'
        | EmptySeat -> 'L'
        | OccupiedSeat -> '#')
    >> Array.chunkBySize (90)
    >> Array.collect (Array.append [| '\n' |])
    >> System.String

let calcOccupied =
    Array.filter ((=) OccupiedSeat) >> Array.length

// run (parse |>> getAdjacentSeats 9) """L.LL.LL.LL
// LLLLLLL.LL
// L.L.L..L..
// LLLL.LL.LL
// L.LL.LL.LL
// L.LLLLL.LL
// ..L.L.....
// LLLLLLLLLL
// L.LLLLLL.L
// L.LLLLL.LL"""

// [0..100] |> List.chunkBySize 10

// [[0;  1;  2;  3;  4;  5;  6;  7;  8;  9];
// [10; 11; 12; 13; 14; 15; 16; 17; 18; 19];
// [20; 21; 22; 23; 24; 25; 26; 27; 28; 29];
// [30; 31; 32; 33; 34; 35; 36; 37; 38; 39];
// [40; 41; 42; 43; 44; 45; 46; 47; 48; 49];
// [50; 51; 52; 53; 54; 55; 56; 57; 58; 59];
// [60; 61; 62; 63; 64; 65; 66; 67; 68; 69];
// [70; 71; 72; 73; 74; 75; 76; 77; 78; 79];
// [80; 81; 82; 83; 84; 85; 86; 87; 88; 89];
// [90; 91; 92; 93; 94; 95; 96; 97; 98; 99];]

// String.length "LLLLL.LLLLLLL.LLLLLL.L.LLLL..LLLL.LLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLL.L.LLLLLLLLLLLLLL.LLLLLL"

let solve input =
    match run parse input with
    | Success (map, _) -> map |> simulate |> calcOccupied
    | Failure _ -> 0

solve input |> printfn "Solution %A"
