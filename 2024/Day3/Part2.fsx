#r "nuget: FSharpPlus"
#r "nuget: Unquote"

open FSharpPlus
open System.IO

let (|Integer|_|) input =
    if (String.contains ' ' input) then
        None
    else
        tryParse<int> input

let parse opening closing input =
    let endIndex =
        input
        |> String.tryFindSliceIndex closing
        |> Option.map (fun index -> index - 1)
        |> Option.defaultWith (fun () -> String.length input - 1)

    let startIndex =
        input[0..endIndex]
        |> String.tryFindLastSliceIndex opening
        |> Option.map (fun index -> index + String.length opening)
        |> Option.defaultWith (fun () -> endIndex)

    let content = input[startIndex..endIndex]

    let remainder = input[(endIndex + 2)..]

    content, remainder

module Pair =
    let ofList = function
        | [ Integer x; Integer y ] -> Some (x, y)
        | _ -> None

    let multiply (x, y) = x * y

let rec calculate accumulation input =
    let content, remainder = input |> parse "mul(" ")"

    let product =
        content
        |> String.split [","]
        |> Seq.toList
        |> Pair.ofList
        |> Option.map Pair.multiply
        |> Option.defaultValue 0

    match remainder with
    | "" ->
        product + accumulation
    | _ ->
        remainder
        |> calculate product 
        |> (+) accumulation

let split input =
    let trim part = 
        let endIndex =
            part
            |> String.tryFindSliceIndex "don't()"
            |> Option.defaultValue (String.length part - 1)

        part[0..endIndex]

    input 
    |> String.split ["do()"]
    |> Seq.map trim

$"{__SOURCE_DIRECTORY__}/Input.txt"
|> File.ReadAllText
|> split
|> Seq.map (fun x -> x |> calculate 0)
|> Seq.reduce (+)
