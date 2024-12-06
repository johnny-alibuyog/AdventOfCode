#r "nuget: FSharpPlus"

open FSharpPlus
open System.IO

let path = $"{__SOURCE_DIRECTORY__}/Part1.txt"

let readFile path = path |> File.ReadAllLines |> Seq.toList

let splitToInt (line: string) =
    line
    |> String.split [ "   " ]
    |> Seq.map int
    |> Seq.toList

let input = path |> readFile |> List.map splitToInt

let (list1, list2) = (input |> List.map List.head, input |> List.map List.last)

let totalDistance (list1: int list) (list2: int list) =
    let orderedList1 = list1 |> List.sortDescending
    let orderedList2 = list2 |> List.sortDescending

    (orderedList1, orderedList2)
    ||> List.zip
    |> List.map (fun (x, y) -> abs (x - y))
    |> List.sum

totalDistance list1 list2
