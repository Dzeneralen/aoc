open System.IO

// PART 1
let lines = File.ReadAllLines("./day2/input.txt")

type Round = { Red: int; Blue: int; Green: int; }
type Game = { Id: int; Rounds: Round[] }

let parseGameNumber (str: string) =
    match str.Split(" ") with
    | [|_; numStr|] -> int numStr
    | _ -> failwithf "Unknown game number from: %s" str

let parseRound (round: string) =
    round.Split(",")
    |> Array.map (fun x -> x.Trim())
    |> Array.fold (fun acc next ->
        match next.Split(" ") with
        | [| amountStr; colorStr |] ->
            let amount = int amountStr
            match colorStr with
            | "red" -> { acc with Red = acc.Red + amount }
            | "green" -> { acc with Green = acc.Green + amount }
            | "blue" -> { acc with Blue = acc.Blue + amount }
            | _ -> failwithf "Unknown color in round: %s" colorStr
        | _ -> failwithf "Unknown round format: %s" round
        ) { Red = 0; Green = 0; Blue = 0 }

let parseGame (line: string) =
    let gameText, roundsText =
        match line.Split(':') with
        | [| gameText; roundsText |] -> gameText, roundsText
        | _ -> failwithf "Unknown game format: %s" line
    
    let gameNumber = parseGameNumber gameText
    let rounds =
        roundsText.Split(";")
        |> Array.map parseRound
        
    { Id = gameNumber; Rounds = rounds } 
   
let possibleRound full partial =
    let leftoverRed = full.Red - partial.Red
    let leftoverGreen = full.Green - partial.Green
    let leftoverBlue = full.Blue - partial.Blue
    
    leftoverRed >= 0 && leftoverGreen >= 0 && leftoverBlue >= 0

let possibleGame totalPieces game =
    game.Rounds
    |> Array.forall (possibleRound totalPieces)


let fullBag = { Red = 12; Green = 13; Blue = 14 }
let result =
    lines
    |> Array.map parseGame
    |> Array.filter (possibleGame fullBag)
    |> Array.sumBy (fun x -> x.Id)
    
// PART 2
let minimumSetOfCubesForValidGame game =
    game.Rounds
    |> Array.fold (fun acc next ->
        {
            Red = if acc.Red > next.Red then acc.Red else next.Red
            Green = if acc.Green > next.Green then acc.Green else next.Green
            Blue = if acc.Blue > next.Blue then acc.Blue else next.Blue
        }
        ) { Red = 0; Green = 0; Blue = 0 }
    
let result2 =
    lines
    |> Array.map parseGame
    |> Array.map minimumSetOfCubesForValidGame
    |> Array.map (fun x -> x.Red * x.Green * x.Blue)
    |> Array.sum
