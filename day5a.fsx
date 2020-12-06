(*
    Find highest available seat
*)

open System.IO

let input = File.ReadAllText "./day5-input.txt"

let lines (str: string) = str.Split '\n' |> List.ofArray

let toBinary: (string -> string) =
    Seq.fold (fun bin c -> bin + (if c = 'B' || c = 'R' then "1" else "0")) ""

let binaryToSeatId bin = System.Convert.ToInt32(bin, 2)

let toSeatId = toBinary >> binaryToSeatId

let solve = lines >> List.map toSeatId >> List.max

solve input |> printfn "Solution %A"
