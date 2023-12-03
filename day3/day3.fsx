open System
open System.IO
let lines = File.ReadAllLines("./day3/input.txt")

// PART 1
let getCoordinatesForAreaAround x y =
    //   xxx
    //   xxx
    //   xxx
    [
        x,y // center
        x+1,y // right
        x+1,y+1 // upper right corner
        x,y+1 // up
        x-1,y+1 // upper left corner
        x-1,y; // left
        x-1,y-1; // lower left corner
        x,y-1; // down
        x+1,y-1; // lower right corner
    ]

let findMachineParts (input: string[]) =
    let mutable acceptedNumbers = []
    let mutable accumulator = ""
    let mutable included = false

    // row index i, column index j
    let rowCount, colCount = Array.length input, input[0].Length

    for i = 0 to rowCount - 1 do
        for j = 0 to colCount - 1 do
            let c = input[i][j]
            
            if Char.IsDigit c then
                accumulator <- accumulator + string c
                
                if not included then
                    let validCoords =
                        getCoordinatesForAreaAround i j
                        |> List.filter (fun (x, _) -> rowCount > x && x >= 0) 
                        |> List.filter (fun (_, y) -> colCount > y && y >= 0)
                    
                    let hasAdjacentSymbol =
                        validCoords
                        |> List.exists (fun (x, y) ->
                            let c = input[x][y]
                            not (Char.IsDigit c || c = '.')
                            )
                        
                    included <- hasAdjacentSymbol
            else
                if accumulator <> "" && included then
                    acceptedNumbers <- accumulator :: acceptedNumbers
                
                accumulator <- ""
                included <- false
    
    if accumulator <> "" && included then
        acceptedNumbers <- accumulator :: acceptedNumbers
    
    acceptedNumbers


let result =
    lines
    |> findMachineParts 
    |> List.rev
    |> List.map int
    |> List.sum
