open System.IO

let readFile (filename : string) : int [,] =
    filename
    |> File.ReadLines
    |> Seq.map (Seq.map (string >> int) >> Seq.toArray)
    |> Seq.toList
    |> array2D

let input = readFile "input.txt"

// Stolen from Ken to make a neater and faster version of my own code
let neighbours i j grid =
    seq {for x in i-1 .. i+1 do
         if 0 <= x && x < Array2D.length1 grid then
            for y in j-1 .. j+1 do
            if 0 <= y && (x,y) <> (i,j) && y < Array2D.length2 grid then yield x,y}

let makeStep (octopodes : int [,]) : int [,] * int =
    let len1 = Array2D.length1 octopodes
    let len2 = Array2D.length2 octopodes
    let incr = Array2D.map ((+) 1) octopodes

    let rec floodFlashes (incr : int [,]) (flashed : bool [,]) =
        let initFun i j =
            incr
            |> neighbours i j
            |> Seq.filter (fun (x,y) -> incr.[x,y] > 9 && not flashed.[x,y])
            |> Seq.length
            |> (+) incr.[i,j]

        let newOctopodes = Array2D.init len1 len2 initFun
        let newFlashed = Array2D.map ((<) 9) incr
        let allToFlash = Array2D.map ((<) 9) newOctopodes

        if newFlashed = allToFlash then
            let retArr = Array2D.map (fun x -> if x > 9 then 0 else x) newOctopodes
            let count =
                retArr
                |> Seq.cast
                |> Seq.filter ((=) 0)
                |> Seq.length
            (retArr, count)
        else
            floodFlashes newOctopodes newFlashed

    floodFlashes incr <| Array2D.create len1 len2 false

let makeNSteps (octopodes : int [,]) N : int =
    let rec stepHelper octopodes count n =
        match n with
        | 0 -> count
        | n ->
            let (newOctopodes, numFlashes) = makeStep octopodes
            stepHelper newOctopodes (count + numFlashes) (n-1)

    stepHelper octopodes 0 N

let findFirstSync (octopodes : int [,]) : int =
    let allFlashVal = Array2D.length1 octopodes * Array2D.length2 octopodes
    let rec stepHelper octopodes n =
        let (newOctopodes, numFlashes) = makeStep octopodes
        if numFlashes = allFlashVal then
            n
        else
            stepHelper newOctopodes (n+1)
    stepHelper octopodes 1

printfn "%A" <| makeNSteps input 100
printfn "%A" <| findFirstSync input
