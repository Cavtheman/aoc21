open System
open System.IO

type line = int * int * int * int

let toLine (lst : int list) : line =
    match lst with
    | [x1;y1;x2;y2] -> (x1, y1, x2, y2)
    | _ -> failwith "Input has too many elements"


let splitter (s : string) : line =
    s.Split ([|"->"; " "|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun x -> x.Split ',' |> Seq.map int)
    |> Seq.reduce Seq.append
    |> Seq.toList
    |> toLine

let horVerFilter ((x1, y1, x2, y2) : line) : bool =
    x1 = x2 || y1 = y2

let getLinePoints ((x1, y1, x2, y2) : line) : seq<int * int> =
    if x1 = x2 then // Horizontal
        let startY = min y1 y2
        let endY   = max y1 y2
        seq { for y in startY..endY do yield (x1, y) }
    elif y1 = y2 then // Vertical
        let startX = min x1 x2
        let endX   = max x1 x2
        seq { for x in startX..endX do yield (x, y1) }
    else //Diagonal
        let slopeX = if x1 > x2 then -1 else 1
        let slopeY = if y1 > y2 then -1 else 1
        seq { for (x,y) in Seq.zip [x1..slopeX..x2] [y1..slopeY..y2] do yield (x,y) }



let readFile (filename : string) =
    filename
    |> File.ReadLines
    |> Seq.map splitter
    //|> Seq.filter horVerFilter // Uncomment for part 1
    |> Seq.map getLinePoints
    |> Seq.concat
    |> Seq.countBy id
    |> Seq.filter (fun (_, count) -> count > 1)
    |> Seq.length


let filename = "input.txt"
let input = readFile filename
printfn "%A" input
