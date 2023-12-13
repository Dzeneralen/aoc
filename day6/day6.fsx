open System.IO

let lines = File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")

// PART 1

let getNumbers (items: string array) =
    items
    |> Array.filter (fun x -> fst (System.Int32.TryParse x))
    |> Array.map int

let times =
    lines[0].Split " "
    |> getNumbers

let distances =
    lines[1].Split " "
    |> getNumbers

let options = 
    Array.zip times distances
    |> Array.map (fun (time, distance) -> 
        Array.init time (fun x -> (x, time - x))
        |> Array.map (fun (speed, timeLeft) -> 
            speed * timeLeft
        )
        |> Array.filter (fun x -> x > distance)
        |> Array.length
    )

let result = options |> Array.fold (*) 1