(*
    Find the empty seat
*)

open System.IO

let input = File.ReadAllText "./day5-input.txt"

let lines (str: string) = str.Split '\n' |> List.ofArray

let toBinary: (string -> string) =
    Seq.fold (fun bin c -> bin + (if c = 'B' || c = 'R' then "1" else "0")) ""

let toSeatId code = System.Convert.ToInt32(toBinary code, 2)

let findSeat seats =
    let rec search ids =
        match ids with
        // If the difference between x & y is greater than 1 the seat id is available
        | x :: y :: tail -> if y - x = 2 then x + 1 else search (y :: tail)
        | _ -> 0

    search (List.sort seats)

let solve = lines >> List.map toSeatId >> findSeat

solve input |> printfn "Solution %A"
