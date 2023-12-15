open System.IO

type Hand = { Hand: char list; Bid: int }

let hands = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")
    |> Array.map (fun line -> 
        match line.Split " " with
        | [| hand; bid |] -> { Hand = hand |> Seq.toList; Bid = int bid }
        | _ -> failwith $"Invalid input: {line}"
    )

let numberOfSame num chars =
    chars
    |> Array.groupBy (fun x -> x)
    |> Array.tryFind (fun g -> g |> snd |> Array.length = num)
    |> Option.map (fun (char, _) -> char)

let (|FiveOfAKind|_|) chars = 
    numberOfSame 5 chars

let (|FourOfAKind|_|) chars =
    numberOfSame 4 chars

let (|ThreeOfAKind|_|) chars =
    numberOfSame 3 chars

let (|TwoOfAKind|_|) chars =
    numberOfSame 2 chars

let (|HighestCard|) chars =
    HighestCard Array.head chars

let label chars =
    match chars with
    | FiveOfAKind x -> printfn $"Five of a kind of {x}!"
    | FourOfAKind x -> printfn $"Four of a kind of {x}!"
    | ThreeOfAKind x -> printfn $"Three of a kind of {x}!"
    | TwoOfAKind x -> printfn $"Two of a kind of {x}!"
    | HighestCard x -> printfn $"highest card is {x}!"
    | _ -> printfn "WTF"

label [|'B'; 'A'; 'A'; 'C'; 'B'|]