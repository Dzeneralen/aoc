open System
open System.IO

// PART 1

let getNumbers (str: string) : string =
    str
    |> Seq.filter Char.IsDigit
    |> String.Concat
    
let getFirstAndLast (input: string) =
    match input.Length with
    | 1 -> String.replicate 2 input
    | 2 -> input
    | _ -> sprintf "%c%c" input[0] input[input.Length - 1]
    
let lines = File.ReadAllLines("./day1/input.txt")

let answer =
    lines
    |> Array.map getNumbers
    |> Array.map getFirstAndLast
    |> Array.map int
    |> Array.sum

// PART 2

// need to re-add the word since other numbers
// may be using it to form their own version
// ex. "oneight" can be "1ight" or "on8" depending
// on order of replaces
let numbers = [
    "one",   "one1one";
    "two",   "two2two";
    "three", "three3three";
    "four",  "four4four";
    "five",  "five5five";
    "six",   "six6six";
    "seven", "seven7seven";
    "eight", "eight8eight";
    "nine",  "nine9nine";
]

let textToNum (str: string) : string =
    let replacer (str: string) (tuple: string * string) =
        str.Replace(fst tuple, snd tuple)
    
    List.fold replacer str numbers
    
let secondAnswer =
    lines
    |> Array.map textToNum
    |> Array.map getNumbers
    |> Array.map getFirstAndLast
    |> Array.map int
    |> Array.sum