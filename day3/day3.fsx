open System.IO

// Helper functions lifted straight from PoP
// Lists are probably a horrible way of handling this
let notEmpty (llst : 'a list list) = not (List.isEmpty llst) && List.forall (fun (lst : 'a list) -> not lst.IsEmpty) llst

let firstColumn (llst: 'a list list) : 'a list =
    if notEmpty llst then
        List.map List.head llst
    else
        []

let dropFirstColumn (llst : 'a list list) : 'a list list =
    if notEmpty llst && llst.Length > 1 && (llst.Item 0).Length > 1 then
        List.map List.tail llst
    else
        []

let rec transposeLstLst (llst : 'a list list) : 'a list list =
    match llst with
    | []   -> []
    | llst -> (firstColumn llst) :: (transposeLstLst (dropFirstColumn llst))


let fromBin lst =
    let len = (List.length lst) - 1
    lst |> List.mapi (fun i x -> int <| float x * 2.**float (len - i)) |> List.sum

let binDiagnostics (filename : string) : int * int =
    let len = filename |> File.ReadLines |> Seq.length
    let sumList =
        filename
        |> File.ReadLines
        |> Seq.map (Seq.toList << Seq.map (fun x -> x |> string |> int))
        |> Seq.toList
        |> transposeLstLst
    let gamma = sumList |> List.map (fun x -> if List.sum x >= len/2 then 1 else 0) |> fromBin
    let epsilon = sumList |> List.map (fun x -> if List.sum x >= len/2 then 0 else 1) |> fromBin
    (gamma, epsilon)


let gamma, epsilon = binDiagnostics "input.txt"
printfn "Power consumption: %A" <| gamma * epsilon

// Part 2
let readFile (filename : string) : int [] [] =
    filename
    |> File.ReadLines
    |> Seq.map (Seq.map (fun x -> x |> string |> int) >> Seq.toArray)
    |> Seq.toArray

let fromBinArr arr =
    let len = (Array.length arr) - 1
    arr |> Array.mapi (fun i x -> int <| float x * 2.**float (len - i)) |> Array.sum

let flip f a b = f b a

let getMostCommon (bitArr : int [] []) (i : int) : int =
    let iBitArrSum = bitArr |> Array.map (flip Array.get i) |> Array.sum
    // Converting values to floats is the easiest way here to handle rounding
    if float iBitArrSum >= float (Array.length bitArr) / 2. then 1 else 0

let getLeastCommon (bitArr : int [] []) (i : int) : int =
    match getMostCommon bitArr i with
    | 0 -> 1
    | 1 -> 0
    | _ -> failwith "getMostCommon returned wrong value"

let filename = "input.txt"
//let filename = "inputSimple.txt"
let input = readFile filename

let lifeSupportRating (bitArr : int [] []) =
    let rec bitCriteria criteriaFunc (i : int) (bitArr : int [] []) : int [] =
        match Array.length bitArr with
        | 0 -> failwith "Filtered entire array"
        | 1 -> Array.head bitArr
        | _ ->
            bitArr
            |> Array.filter (fun elem -> elem.[i] = criteriaFunc bitArr i)
            |> bitCriteria criteriaFunc (i+1)

    let oxyGenRating = bitCriteria getMostCommon 0 bitArr
    let co2GenRating = bitCriteria getLeastCommon 0 bitArr
    fromBinArr oxyGenRating * fromBinArr co2GenRating


printfn "Life support rating: %A" <| lifeSupportRating input
