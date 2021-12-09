open System.IO

let readFile (filename : string) : int [,] =
    filename
    |> File.ReadLines
    |> Seq.map (Seq.map (string >> int) >> Seq.toArray)
    |> Seq.toList
    |> array2D

let input = readFile "input.txt"

let findLowPoints (heightMap : int [,]) : (int * int) list =
    let mutable lowPoints = []
    //let adjacents = [(1,1); (1,0); (1,-1); (0,1); (0,-1); (-1,1); (-1,0); (-1,-1)]
    let adjacents = [(1,0); (-1,0); (0,1); (0,-1)]
    let isLowPoint i j =
        let potMin = heightMap.[i,j]
        // If at the edge of map the default value is 10, since all other values are only a single digit
        // Doing this using try ... with ... is mostly just me being lazy
        let adjacentHeights = List.map (fun (iD, jD) -> try heightMap.[i+iD,j+jD] with _ -> 10) adjacents
        List.forall (fun x -> x > potMin) adjacentHeights

    for x in 0..Array2D.length1 heightMap - 1 do
        for y in 0..Array2D.length2 heightMap - 1 do
            if isLowPoint x y then
                lowPoints <- (x,y) :: lowPoints
    lowPoints


let getRiskLevel (heightMap : int [,]) : int =
    let lowPoints = findLowPoints heightMap
    lowPoints
    |> List.map (fun (x,y) -> 1 + heightMap.[x,y])
    |> List.sum


printfn "%A" <| getRiskLevel input
