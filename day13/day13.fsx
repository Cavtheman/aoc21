open System.IO

//type paper = seq<int * int>
type paper = list<int * int>
type fold = string * int

let flip f a b = f b a
let splitByFun (f : 'a -> bool) (lst : 'a list) : 'a list * 'a list =
    lst |> List.findIndex f |> flip List.splitAt lst

let strSplit (c : char) (s : string) : string [] = s.Split c

let readFile (filename : string) : fold list * paper =
    let (dots, _::folds) =
        filename
        |> File.ReadLines
        |> Seq.toList
        |> splitByFun ((=) "")

    let foldAxes =
        folds
        |> Seq.map ((fun x -> x.[11..]) >> strSplit '=' >> fun [|ax;pos|] -> (ax,int pos))

    let dotSeq = Seq.map (strSplit ',' >> Array.map int >> fun [|x;y|] -> (x,y)) dots

    (Seq.toList foldAxes, Seq.toList dotSeq)

let toStr (lst : char list ) : string = List.fold (fun acc x -> string x + acc) "" lst

let printDotArr ((height, width) : int * int) (dots : paper) : unit =
    let initFun i j =
        match Seq.tryFind ((=) (j,i)) dots with
        | None -> ' '
        | Some _ -> '#'

    let arr = Array2D.init height width initFun
    for i in 0..height-1 do
        arr.[i,0..width-1]
        |> Seq.cast<char>
        |> Seq.rev
        |> Seq.toList
        |> toStr
        |> printfn "%s"


let makeFold ((ax, pos) : fold) (dots : paper) : paper =
    let mapFun (x,y) =
        match ax with
        | "y" -> if y > pos then (x, 2*pos - y) else (x, y)
        | "x" -> if x > pos then (2*pos - x, y) else (x, y)
        | _   -> failwith "Invalid fold direction"
    dots
    |> Seq.map mapFun
    |> Seq.distinct
    |> Seq.toList

let rec makeAllFolds (folds : fold list) (dots : paper) : paper =
    match folds with
    | [] -> dots
    | x::xs -> makeFold x dots |> makeAllFolds xs

let (folds, dots) = readFile "input.txt"
let singleFold = makeFold (List.head folds) dots
let allFolds = makeAllFolds folds dots

printfn "Number of dots: %A" <| List.length singleFold
printDotArr (6, 40) allFolds
