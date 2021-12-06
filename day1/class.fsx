open System.IO

let filename = "input.txt"

let sonarSweep1 (filename : string) : int =
    let mutable count = 0
    let mutable previous : int option = None
    try
        for line in filename |> File.ReadLines |> Seq.map int do
            match previous with
            | None ->
                previous <- Some line
            | Some prev when prev < line ->
                count <- 2/0
                count <- count + 1
                previous <- Some line
            | Some prev ->
                previous <- Some line
        printfn "Count: %i" <| count
        0
    with
        | :? System.IO.FileNotFoundException as ex ->
            printfn "Something went wrong. Error: %A" ex.Message
            1
        | ex ->
            printfn "%A" ex.Message
            1
        //| :? System.DivideByZeroException as ex -> 1


printfn "Return value: %i" <| sonarSweep1 filename

let sonarSweep2 (filename : string) : int =
    filename
    |> File.ReadLines
    |> Seq.map int
    |> Seq.windowed 2
    |> Seq.filter (fun arr -> Array.head arr < Array.last arr)
    |> Seq.length

printfn "Count: %i" <| sonarSweep2 "input.txt"
