#load "Parser.fsx"
open System.IO
open Parser

let input = File.ReadAllText "./day12-input.txt"

type Distance = int
type Times = int

type Instruction =
    | MoveNorth of Distance
    | MoveEast of Distance
    | MoveSouth of Distance
    | MoveWest of Distance
    | MoveForward of Distance
    | TurnLeft of Times
    | TurnRight of Times

type Position = int * int

let degreeToTimes degrees = (abs (degrees / 90))

let pint =
    many1 (anyOf [ '0' .. '9' ])
    |>> charListToString
    |>> int

let pnorth = pchar 'N' >>. pint |>> MoveNorth
let peast = pchar 'E' >>. pint |>> MoveEast
let psouth = pchar 'S' >>. pint |>> MoveSouth
let pwest = pchar 'W' >>. pint |>> MoveWest
let pforward = pchar 'F' >>. pint |>> MoveForward

let pleft =
    pchar 'L' >>. pint |>> degreeToTimes |>> TurnLeft

let pright =
    pchar 'R' >>. pint |>> degreeToTimes |>> TurnRight

let pinstruction =
    choice [ pnorth
             peast
             psouth
             pwest
             pforward
             pleft
             pright ]

let parse =
    many1 (pinstruction .>> opt (pchar '\n'))

let turnLeft times waypoint =
    let rec rotate n (wx, wy) =
        if n > 0 then rotate (n - 1) (wy * -1, wx) else (wx, wy)

    rotate times waypoint

let turnRight times waypoint =
    let rec rotate n (wx, wy) =
        if n > 0 then rotate (n - 1) (wy, wx * -1) else (wx, wy)

    rotate times waypoint

let followWaypoint (plane, (wx, wy)) distance =
    let rec inner n (x, y) =
        if n > 0 then inner (n - 1) (x + wx, y + wy) else ((x, y), (wx, wy))

    inner distance plane

let move (plane, (wx, wy)) instruction =
    match instruction with
    | MoveNorth distance -> (plane, (wx, wy + distance))
    | MoveEast distance -> (plane, (wx + distance, wy))
    | MoveSouth distance -> (plane, (wx, wy - distance))
    | MoveWest distance -> (plane, (wx - distance, wy))
    | MoveForward distance -> followWaypoint (plane, (wx, wy)) distance
    | TurnLeft times -> (plane, turnLeft times (wx, wy))
    | TurnRight times -> (plane, turnRight times (wx, wy))

let solve input =
    match run parse input with
    | Success (instruction, _) ->
        instruction
        |> List.fold move ((0, 0), (10, 1))
        |> fst
        |> fun (x, y) -> x + y
    | Failure _ -> 0

solve input |> printfn "Solution %A"
