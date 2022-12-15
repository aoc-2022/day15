open System.IO
open System

// █
let (|Int|_|) (s: string) =
    match Int64.TryParse s with
    | true, value -> Some value
    | false, _ -> None

let input = File.ReadAllLines "/tmp/aoc/input" |> Array.toList

type Pos = int64 * int64

type Sensor(loc: Pos, closest: Pos) =
    member this.Loc = loc
    member this.X = fst loc
    member this.Y = snd loc

    member this.Dist =
        let (x, y), (x2, y2) = loc, closest
        abs (x - x2) + abs (y - y2)

    override this.ToString() = $"Sensor({loc},closest={closest}) "

    member this.Closest(s: Sensor list) =
        let rec closest (d: int64) (s: Sensor list) =
            match (loc, s) with
            | _, [] -> d
            | _, other :: rest when other = this -> closest d rest
            | (x, y), other :: rest ->
                let nd = abs ((fst other.Loc) - x) + abs ((snd other.Loc) - y)
                closest (min d nd) rest

        closest 10000000l s

let parse (s: string) : Sensor =
    let s = s.Split [| ','; '='; ':' |] |> Array.toList

    match s with
    | [ "Sensor at x"; Int x; " y"; Int y; _; Int x2; _; Int y2 ] -> Sensor((x, y), (x2, y2))
    | _ ->
        printfn $"ERROR: ({s})"
        s |> List.map (printfn "%A")
        Sensor((-1, -1), (-1, -1))

let sensors = input |> List.map parse

sensors |> List.map (printfn "%A")

let segmentOnLine (y: int64) (sensor: Sensor) : Option<int64 * int64> =
    let dy = abs (sensor.Y - y)
    let dx = (sensor.Dist - dy)

    if dx < 0 then
        // printfn $"segmentOnLine {y} {sensor} (dy={dy},dx={dx}) -> None"
        None
    else
        let x1 = sensor.X - dx
        let x2 = sensor.X + dx
        // printfn $"segmentOnLine {y} {sensor} -> {(x1, x2)} dy={dy} dx={dx}"

        Some((x1, x2))


let rec mergeSegments (segments: (int64 * int64) list) =
    // printfn $"MERGE {segments}"
    match segments with
    | [] -> []
    | [ s ] -> [ s ]
    | (x1, x2) :: (x3, x4) :: rest when x1 = x3 ->
        // printfn $"A-> {segments}"
        mergeSegments ((x1, max x2 x4) ::rest)
    | (x1, x2) :: (x3, x4) :: rest when x3 <= (x2 + 1L) ->
        // printfn $"B-> {segments}"
        mergeSegments ((x1, max x2 x4) :: rest)
    | (x1, x2) :: (x3, x4) :: rest when x3 > (x2 + 1L) ->
        // printfn $"C-> {segments}"
        (x1, x2) :: mergeSegments ((x3, x4) :: rest)


let length (x1, x2) = x2 - x1 + 1L

let xlines () =
    sensors
    |> List.map (segmentOnLine 2000000)
    |> List.filter Option.isSome
    |> List.map Option.get
    |> List.sort
    |> mergeSegments
    |> List.map length
    |> List.sum

let xlines_test () =
    sensors
    |> List.map (segmentOnLine 10)
    |> List.filter Option.isSome
    |> List.map Option.get
    |> List.sort
//    |> mergeSegments
//    |> List.map length
//    |> List.sum

printfn $"xlines={xlines}"
printfn $"xlines_test={xlines_test ()}"

// xlines |> List.map (printfn "%A")

let covered (y: int) =
    sensors
    |> List.map (segmentOnLine y)
    |> List.filter Option.isSome
    |> List.map Option.get
    |> List.sort
    |> mergeSegments

let rec xcut (maxx: int64) (s: (int64 * int64) list) : (int64 * int64) list =
    // printfn $"xcut {maxx} {s}"

    match s with
    | [] -> []
    | (x1, _) :: _ when x1 > maxx -> []
    | (_, x2) :: rest when x2 < 0 -> xcut maxx rest
    | (x1, x2) :: rest ->
        let newseg = (max x1 0l, min x2 maxx)
        // printfn $"≤ {(x1, x2)} -> {newseg}"
        (max x1 0l, min x2 maxx) :: (xcut maxx rest)

let ys () =
    [ 0..20 ]
    |> List.map covered
    |> List.map List.sort
    |> List.map (xcut 20)
    |> List.sort
    |> List.map (xcut 20L)
    |> List.sort
    |> List.map mergeSegments


let segsOnLine (lineNo: int) =
    sensors
    |> List.map (segmentOnLine lineNo)
    |> List.filter Option.isSome
    |> List.map Option.get
    |> List.sort
    |> xcut 4000000L 
    |> List.sort
    |> mergeSegments

printfn "XXXXXX"

let yr = seq { for i in 0 .. 4000000 -> i }

let ysegs = yr |> Seq.toList |> List.map (fun y -> y,(segsOnLine y))
               |> List.filter (fun (y,s) -> s.Length > 1)
               |> List.head 

let y = ysegs |> fst |> int64
let x = ysegs |> snd |> List.head |> snd |> (fun x -> x + 1L)

let tuning = 4000000L * x + y

printfn $"{ysegs} {y} {x} {tuning}"
    
    // let y = ys |> List.map List.length |> List.findIndex (fun i -> i > 1)
    // let yline = ys[y]
    // |> List.map (List.map length)

    // let x = yline.Head |> snd |> (fun x -> x + 1L)

    // printfn $"y->yline => {y}->{yline} x={x}"

// ys |> printfn "%A"
