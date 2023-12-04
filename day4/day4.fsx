open System.IO

// PART 1

let lines = File.ReadAllLines("./day4/input.txt")

type Game = {
    Number: int
    WinningNumbers: Set<int>
    ActualNumbers: Set<int>
}

let parseCardNumber (input: string) =
    let parts = input.Split(" ")
    parts[parts.Length - 1] |> int

let parseNumbers (input: string) =
    input.Split(" ")
    |> Array.filter(fun x -> System.String.IsNullOrWhiteSpace(x) = false)
    |> Array.map int

let parseCardNumbers (input: string) =
    match input.Split("|") with
    | [| winningNumsStr; actualNumsStr |] ->
        parseNumbers winningNumsStr, parseNumbers actualNumsStr
    | _ -> failwith $"Unknown game format: {input}"

let parseLine (line: string) =
    match line.Split(":") with
    | [| cardStr; gameStr |] ->
        let cardNumber = parseCardNumber cardStr
        let winning, actual = parseCardNumbers gameStr
        
        {
            Number = cardNumber
            WinningNumbers = Set.ofArray winning
            ActualNumbers = Set.ofArray actual 
        }
    | _ -> failwith $"Unknown line format: {line}"
  
let calculateScoreForGame game =
    let cardWinningNumbers = Set.intersect game.WinningNumbers game.ActualNumbers
    pown 2 (cardWinningNumbers.Count - 1)
    
let result =
    lines
    |> Array.map parseLine
    |> Array.map calculateScoreForGame
    |> Array.sum
    
// PART 2
