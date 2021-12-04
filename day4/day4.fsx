open System.IO

type bingoNum = Marked of int | Unmarked of int
type bingoBoard = bingoNum list list

let takeWithTail (lst : 'a list) (i : int) : 'a list * 'a list =
    let headList = List.take i lst
    let tailList = lst.[i..]
    (headList, tailList)

let rec splitToBoards (lst : bingoNum list list) : bingoBoard list =
    let headBoard, tailBoards = takeWithTail lst 5
    match tailBoards with
    | []         -> [headBoard]
    | tailBoards -> headBoard :: splitToBoards tailBoards

let rec transposeLstLst (llst : 'a list list) : 'a list list =
    // First case matches on a list containing at least one list,
    // which itself contains at least one element
    match llst with
    | (_::_)::_ -> List.map List.head llst :: transposeLstLst (List.map List.tail llst)
    | _ -> []

// Makes splitting a string easier
let strSplit (c : char) (s : string) : string [] = s.Split c

let readFile (filename : string) =
    let toBingoNums (line : string) : bingoNum list =
        if Seq.exists ((=) ',') line then []
        else line.Split ' ' |> Array.filter ((<>) "") |> Array.map (int >> Unmarked) |> Array.toList

    let operations =
        filename
        |> File.ReadLines
        |> Seq.take 1
        |> Seq.head
        |> strSplit ','
        |> Array.map int
        |> Array.toList

    let boards =
        filename
        |> File.ReadLines
        |> Seq.map toBingoNums
        |> Seq.filter (fun x -> not (List.isEmpty x))
        |> Seq.toList
        |> splitToBoards
    (operations, boards)

let checkWin (board : bingoBoard) : bool =
    let winFun (line : bingoNum list) : bool =
        let folder acc x =
            match x with
            | Marked _ -> acc && true
            | Unmarked _ -> false
        List.fold folder true line
    let horWin = List.fold (fun acc x -> acc || winFun x) false board
    let vertWin = List.fold (fun acc x -> acc || winFun x) false (transposeLstLst board)
    horWin || vertWin

let drawNum (draw : int) (board : bingoBoard) =
    let rec helper line =
        match line with
        | [] -> []
        | (Unmarked x)::xs when x = draw -> Marked x :: helper xs
        | x::xs -> x :: helper xs
    List.map helper board

let rec findWinner (draws : int list) (boards : bingoBoard list) =
    match draws with
    | []    -> failwith "No winners"
    | x::xs ->
        let markedBoards = List.map (drawNum x) boards
        let maybeWinners = List.filter checkWin markedBoards
        match maybeWinners with
        | []  -> findWinner xs markedBoards
        | [winner] -> (x, winner)
        | _   -> failwith "Multiple winners"

let rec findLastWinner (draws : int list) (boards : bingoBoard list) =
    match draws with
    | []    -> failwith "No winners"
    | x::xs ->
        let markedBoards = List.map (drawNum x) boards
        let losers = List.filter (fun x -> not (checkWin x)) markedBoards
        match markedBoards with
        | [] -> failwith "No losers"
        | [lastWinner] when checkWin lastWinner -> (x, lastWinner)
        | x -> findLastWinner xs losers

let sumUnmarked (board : bingoBoard) : int =
    let filterFun (x : bingoNum) : bool =
        match x with
        | Marked _ -> false
        | Unmarked _ -> true
    let unpack (x : bingoNum) : int=
        match x with
        | Marked a   -> a
        | Unmarked a -> a
    let helper (line : bingoNum list) : int =
        line
        |> List.filter filterFun
        |> List.map unpack
        |> List.sum
    board
    |> List.map helper
    |> List.sum

let filename = "input.txt"
let draws, boards = readFile filename
//let winningBoard = (List.map (fun (Unmarked x) -> Marked x) boards.[0].[0]) :: List.tail boards.[0]

let lastDraw, winner = findWinner draws boards
let lastLastDraw, lastWinner = findLastWinner draws boards

printfn "%A" <| (sumUnmarked winner) * lastDraw
printfn "%A" <| (sumUnmarked lastWinner) * lastLastDraw
