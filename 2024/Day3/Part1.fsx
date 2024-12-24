#r "nuget: FSharpPlus"
#r "nuget: Unquote"

open FSharpPlus
open System.IO
open Swensen.Unquote

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

// test <@
//     "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
//     |> calculate 0
//     |> (=) 161
// @>

$"{__SOURCE_DIRECTORY__}/Input.txt"
|> File.ReadAllText
|> calculate 0
