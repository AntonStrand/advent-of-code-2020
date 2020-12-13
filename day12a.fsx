#load "Parser.fsx"
open System.IO
open Parser

let input = File.ReadAllText "./day12-input.txt"

type Distance = int
type Degree = int

type Direction =
    | East
    | South
    | West
    | North

type Instruction =
    | MoveNorth of Distance
    | MoveEast of Distance
    | MoveSouth of Distance
    | MoveWest of Distance
    | MoveForward of Distance
    | TurnLeft of Degree
    | TurnRight of Degree

type Position = Degree * int * int

let pint =
    many1 (anyOf [ '0' .. '9' ])
    |>> charListToString
    |>> int

let pnorth = pchar 'N' >>. pint |>> MoveNorth
let peast = pchar 'E' >>. pint |>> MoveEast
let psouth = pchar 'S' >>. pint |>> MoveSouth
let pwest = pchar 'W' >>. pint |>> MoveWest
let pforward = pchar 'F' >>. pint |>> MoveForward
let pleft = pchar 'L' >>. pint |>> TurnLeft
let pright = pchar 'R' >>. pint |>> TurnRight

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

let toDegrees direction =
    match direction with
    | East -> 0
    | South -> 90
    | West -> 180
    | North -> 270

let turn direction degrees =
    match (degrees + toDegrees direction) % 360 with
    | 0 -> East
    | 90 -> South
    | 180 -> West
    | _ -> North

let moveForward (direction, x, y) distance =
    match direction with
    | East -> (direction, x + distance, y)
    | South -> (direction, x, y + distance)
    | West -> (direction, x - distance, y)
    | North -> (direction, x, y - distance)


let move (direction, x, y) instruction =
    match instruction with
    | MoveNorth distance -> (direction, x, y - distance)
    | MoveEast distance -> (direction, x + distance, y)
    | MoveSouth distance -> (direction, x, y + distance)
    | MoveWest distance -> (direction, x - distance, y)
    | MoveForward distance -> moveForward (direction, x, y) distance
    | TurnLeft degrees -> (turn direction (-degrees), x, y)
    | TurnRight degrees -> (turn direction degrees, x, y)

let solve input =
    match run parse input with
    | Success (instruction, _) ->
        instruction
        |> List.fold move (East, 0, 0)
        |> fun (_, x, y) -> x + y
    | Failure _ -> 0

solve input |> printfn "Solution %d"
