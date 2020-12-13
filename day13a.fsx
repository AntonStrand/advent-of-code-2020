#load "Parser.fsx"
open System.IO
open Parser

let input = File.ReadAllText "./day13-input.txt"

let pint =
    many1 (anyOf [ '0' .. '9' ])
    |>> charListToString
    |>> int

let ptimestamp = pint .>> opt (pchar '\n')

let pbus = pint .>> opt (pchar ',') |>> Some

let poutOfService =
    pchar 'x'
    .>> opt (pchar ',')
    |>> fun _ -> None

let pbuses =
    many1 (pbus <|> poutOfService)
    .>> opt (pchar '\n')
    |>> List.choose id

let parse = ptimestamp .>>. pbuses

let ceil x = int x + 1

let findNextDeparture timestamp bus =
    (bus, bus * ceil (timestamp / bus) - timestamp)

let solve input =
    match run parse input with
    | Success ((timestamp, buses), _) ->
        buses
        |> List.map (findNextDeparture timestamp)
        |> List.minBy snd
        |> fun (x, y) -> x * y
    | Failure _ -> 0

solve input |> printfn "Solution %A"
