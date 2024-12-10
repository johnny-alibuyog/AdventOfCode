#r "nuget: FSharpPlus"
#r "nuget: Unquote"
#r "nuget: Expecto"

open FSharpPlus
open Swensen.Unquote
open System.IO

let path = $"{__SOURCE_DIRECTORY__}\\Input.txt"

let testInput =
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


let isSafe report' =
    let isMonotonic report' =
        let asc = Seq.sort report'
        let desc = Seq.sortDescending report' 

        (Seq.forall2 (=) report' asc) || 
        (Seq.forall2 (=) report' desc)

    let isWithinBounds (x, y) = 
        let diff = abs (x - y)
        1 <= diff && diff <= 3

    let monotonic = report' |> isMonotonic
    let bounds = report' |> Seq.pairwise  |> Seq.forall isWithinBounds
    monotonic && bounds

let isReducedSafe report = 
    let reduce report = 
        seq {
            yield report
            yield! report |> Seq.mapi (fun index _ -> report |> Seq.removeAt index)
        }

    report 
    |> reduce 
    |> Seq.exists isSafe

path
|> File.ReadAllText
|> parseToList
|> List.filter isReducedSafe
|> List.length

test <@
    testInput
    |> parseToList
    |> List.filter isReducedSafe
    |> List.length 
    |> (=) 4
@> 
