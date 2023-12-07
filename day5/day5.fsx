open System.IO

let lines = File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")

// PART 1
type Context =
    | None
    | DataLine
    | Seeds
    | SeedToSoilMap
    | SoilToFertilizerMap
    | FertilizerToWaterMap
    | WaterToLightMap
    | LightToTemperatureMap
    | TemperatureToHumidityMap
    | HumidityToLocationMap
    
type Range = { Destination: int64; Source: int64; Range: int64; }

type State = {
    Seeds: int64 array
    SeedToSoil: Range array
    SoilToFertilizer: Range array
    FertilizerToWater: Range array
    WaterToLight: Range array
    LightToTemperature: Range array
    TemperatureToHumidity: Range array
    HumidityToLocation: Range array
}

let parseSeeds (str: string) =
    str.Split(":")
    |> Array.last
    |> _.Trim()
    |> _.Split(" ")
    |> Array.filter (fun x -> x <> "")
    |> Array.map int64

let parseRange (str: string) =
    match str.Split(" ") with
    | [|dest; src; num|] ->
        {
            Destination = int64 dest
            Source = int64 src
            Range = int64 num
        }
    | _ -> failwith $"Invalid format for map: {str}"

let parseContext (str: string) =
    if str.Contains(":") then
        str.Split(":")
        |> Array.head
        |> function
            | "seeds" -> Seeds
            | "seed-to-soil map" -> SeedToSoilMap
            | "soil-to-fertilizer map" -> SoilToFertilizerMap
            | "fertilizer-to-water map" -> FertilizerToWaterMap
            | "water-to-light map" -> WaterToLightMap
            | "light-to-temperature map" -> LightToTemperatureMap
            | "temperature-to-humidity map" -> TemperatureToHumidityMap
            | "humidity-to-location map" -> HumidityToLocationMap
            | x -> failwith $"Unknown context type: {x}"
    else
        if str.Length = 0 then None else DataLine
        
let parseLinesToState lines =
    let mutable state = {
        Seeds = [||]
        SeedToSoil = [||]
        SoilToFertilizer = [||]
        FertilizerToWater = [||]
        WaterToLight = [||]
        LightToTemperature = [||]
        TemperatureToHumidity = [||]
        HumidityToLocation = [||]
    }
    let mutable current = None
    
    let appendTo arr item =
        Array.append arr [|item|]
    
    for line in lines do
        let context = parseContext line
        
        match context with
        | None -> current <- None
        | Seeds -> state <- { state with Seeds = parseSeeds line }
        | DataLine ->
            match current with
            | SeedToSoilMap ->
                state <- { state with SeedToSoil = appendTo state.SeedToSoil (parseRange line) }
            | SoilToFertilizerMap ->
                state <- { state with SoilToFertilizer = appendTo state.SoilToFertilizer (parseRange line) }
            | FertilizerToWaterMap ->
                state <- { state with FertilizerToWater = appendTo state.FertilizerToWater (parseRange line) }
            | WaterToLightMap ->
                state <- { state with WaterToLight = appendTo state.WaterToLight (parseRange line) }
            | LightToTemperatureMap ->
                state <- { state with LightToTemperature = appendTo state.LightToTemperature (parseRange line) }
            | TemperatureToHumidityMap ->
                state <- { state with TemperatureToHumidity = appendTo state.TemperatureToHumidity (parseRange line) }
            | HumidityToLocationMap ->
                state <- { state with HumidityToLocation = appendTo state.HumidityToLocation (parseRange line) }
            | _ -> failwith $"This context does not support a data line: {context}"
        | x -> current <- x
    
    state

let result = parseLinesToState lines