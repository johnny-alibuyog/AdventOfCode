#r "nuget: FSharpPlus"

open FSharpPlus
open System.IO

let path = $"{__SOURCE_DIRECTORY__}/Input.txt"

let text =
    """
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
    """

let parseToList text =
    let isNotEmpty x = (String.trimWhiteSpaces x) <> ""

    let splitToInt (line: string) =
        line
        |> String.split [ " " ]
        |> Seq.map int
        |> Seq.toList

    text
    |> String.split [ "\n" ]
    |> Seq.filter isNotEmpty
    |> Seq.map splitToInt
    |> Seq.toList

let countSafe report =
    let isMonotonic report' =
        let desc = List.sort report'
        let asc = List.sortDescending report'
        report' = desc || report' = asc

    let isWithinBounds (x, y) = 
        let diff = abs (x - y)
        [1..3] |> List.contains diff

    let isSafe report' =
        let monotonic = report' |> isMonotonic
        let bounds = report' |> List.pairwise  |> List.forall isWithinBounds
        monotonic && bounds

    report |> List.filter isSafe |> List.length

path
|> File.ReadAllText
|> parseToList
|> countSafe

