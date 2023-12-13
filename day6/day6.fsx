open System.IO

let lines = File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")

// PART 1

let getNumbers (items: string array) =
    items
    |> Array.filter (fun x -> fst (System.Int32.TryParse x))

let times =
    lines[0].Split " "
    |> getNumbers

let distances =
    lines[1].Split " "
    |> getNumbers

let getOptions (times: string array) (distances: string array) = 
    Array.zip times distances
    |> Array.map (fun (t, d) -> (int t, int64 d))
    |> Array.map (fun (t, d) -> 
        Array.init t (fun x -> (x, t - x))
        |> Array.map (fun (speed, timeLeft) -> 
            int64 speed * int64 timeLeft
        )
        |> Array.filter (fun x -> x > d)
        |> Array.length
    )

let result =
    getOptions times distances
    |> Array.fold (*) 1

// PART 2

let parseLineAsSingleDigit (line: string) =
    line.Split " "
    |> getNumbers
    |> Array.fold (fun acc next -> 
        acc + string next
    ) ""

let time = lines[0] |> parseLineAsSingleDigit
let distance = lines[1] |> parseLineAsSingleDigit

let result2 = getOptions [|time|] [|distance|]