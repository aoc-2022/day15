open System.IO
open System

// █
let (|Int|_|) (s:string) =
    match Int64.TryParse s with
    | true,value -> Some value
    | false,_ -> None

let input = File.ReadAllLines "/tmp/aoc/input" |> Array.toList


input |> List.map (printfn "%A")    