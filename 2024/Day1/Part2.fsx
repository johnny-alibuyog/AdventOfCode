#r "nuget: FSharpPlus"

open FSharpPlus
open System.IO

let path = $"{__SOURCE_DIRECTORY__}/Input.txt"

let readFile path = path |> File.ReadAllLines |> Seq.toList

let splitToInt (line: string) =
    line
    |> String.split [ "   " ]
    |> Seq.map int
    |> Seq.toList

let input = path |> readFile |> List.map splitToInt

let (list, map) = 
    let list1 = 
        input 
        |> List.map List.head

    let map = 
        input 
        |> List.map List.last
        |> List.groupBy (fun x -> x)
        |> List.map (fun (x, y) -> x, List.length y)
        |> Map.ofList

    (list1, map)
    
let scoreSimilarity (list : int list) (map: Map<int, int>) =
    let getScore key = 
        map 
        |> Map.tryFind key 
        |> Option.defaultValue 0
        |> (*) key

    list
    |> List.map getScore
    |> List.sum

scoreSimilarity list map
