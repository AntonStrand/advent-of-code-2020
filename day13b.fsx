#load "Parser.fsx"
open System.IO
open Parser

(*
    The solution is heavily inspired by @jacobchrismarsh's solution and Chinese Remainder Theorem.
    https://gist.github.com/jacobchrismarsh/485253dbd42da10ad92d6cc03559cd84#file-day13-rs-L39-L59
*)

let input = File.ReadAllText "./day13-input.txt"

let pint =
    many1 (anyOf [ '0' .. '9' ])
    |>> (charListToString >> int64)

let ptimestamp = pint .>> opt (pchar '\n')

let pbus = pint .>> opt (pchar ',') |>> Some

let poffset =
    pchar 'x'
    .>> opt (pchar ',')
    |>> fun _ -> None

let pschedule =
    many1 (pbus <|> poffset) .>> opt (pchar '\n')

let parse =
    ptimestamp
    >>. pschedule
    |>> List.indexed
    |>> List.choose (fun (offset, bus) -> bus |> Option.map (fun id -> (int64 offset, id)))

let findTimestamp buses =
    let rec inner (offset, id) timestamp waitTime =
        if (timestamp + offset) % id = 0L
        then (timestamp, waitTime * id)
        else inner (offset, id) (timestamp + waitTime) waitTime

    let rec loop buses (timestamp, waitTime) =
        match buses with
        | bus :: rest -> inner bus timestamp waitTime |> loop rest
        | [] -> timestamp

    loop buses (1L, 1L)

let solve input =
    match run (parse |>> findTimestamp) input with
    | Success (timestamp, _) -> timestamp
    | Failure _ -> 0L

solve input |> printfn "Solution %d"
