open System
open System.IO
let lines = File.ReadAllLines("./day3/input.txt")

// PART 1

type Number = { Value: string; Row: int; Range: int * int }
type Entry = { Value: string; X: int; Y: int }

let getEntriesFor predicate lines =
    lines
    |> Array.mapi (fun rowIndex column ->
        column
        |> Seq.mapi (fun colIndex char -> { Value = string char; X = rowIndex; Y = colIndex })
        |> Seq.toArray
        )
    |> Array.concat
    |> Array.filter predicate
    
let getNumbersFromLine (row: int) (line: string) =
    let mutable num = ""
    let mutable start = -1
    let mutable numbers = []
    for i = 0 to line.Length - 1 do
        let c = line[i]
        if Char.IsDigit c then
            if start = -1 then start <- i
            num <- num + string c
        else
            if num <> "" then
                numbers <- { Value = num; Row = row; Range = start, i - 1 } :: numbers
            
            num <- ""
            start <- -1
    
    if num <> "" then numbers <- { Value = num; Row = row; Range = start, line.Length - 1 } :: numbers
    
    numbers |> List.rev |> List.toArray  

let findAdjacentNumbers nums gear =
    nums
    |> Array.filter (fun x ->
        let xInside = x.Row + 1 >= gear.X && x.Row - 1 <= gear.X
        let yInside = snd x.Range + 1 >= gear.Y && fst x.Range - 1 <= gear.Y
        
        xInside && yInside
        )

let notDotOrDigit = fun x -> x.Value <> "." && not (Char.IsDigit (char x.Value))
let symbolEntries = getEntriesFor notDotOrDigit lines

let numbers =
    lines
    |> Array.mapi getNumbersFromLine
    |> Array.concat

let result =
    symbolEntries
    |> Array.map (findAdjacentNumbers numbers)
    |> Array.concat
    |> Array.distinct
    |> Array.map (fun x -> int x.Value)
    |> Array.sum

// PART 2
let gears = getEntriesFor (fun x -> x.Value = "*") lines
    
let result2 =
    gears
    |> Array.map (findAdjacentNumbers numbers)
    |> Array.filter (fun x -> x.Length = 2)
    |> Array.map (fun x -> (int x[0].Value) * (int x[1].Value))
    |> Array.sum

  