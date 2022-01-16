open System.IO

type Node = {
    coord : int * int
    cost : int
    mutable dist : int;
    mutable prev : Node option;
    neighbours : list<int * int>
    }

let neighbours i j (grid : 'a [,]) : seq<int * int> =
    seq {for (x,y) in [(i,j+1);(i+1,j);(i,j-1);(i-1,j)] do
         if 0 <= x && x < Array2D.length1 grid && 0 <= y && y < Array2D.length2 grid then
             yield  x,y}

let readFile (filename : string) =
    filename
    |> File.ReadLines
    |> Seq.toList
    |> List.map (Seq.toList >> List.map (string >> int))
    |> array2D

let neighbours2 i j (grid : 'a [,]) : seq<int * int> =
    seq {for (x,y) in [(i,j+1);(i+1,j);(i,j-1);(i-1,j)] do
         if 0 <= x && x < Array2D.length1 grid * 5 && 0 <= y && y < Array2D.length2 grid * 5 then
             yield  x,y}


let part1Read (grid : int [,]) =
    let mapiFun x y v = {coord = (x,y);
                         cost = grid.[x,y];
                         dist = System.Int32.MaxValue;
                         prev = None;
                         neighbours = Seq.toList (neighbours x y grid)}
    let vArr = Array2D.mapi mapiFun grid
    vArr.[0,0].dist <- 0
    let target = (Array2D.length1 vArr - 1, Array2D.length2 vArr - 1)

    let vSet =
        vArr
        |> Seq.cast<Node>
        |> Seq.toList
        |> Set.ofList

    (vSet, target)

let wrap i =

let part2Read (grid : int [,]) =
    let origLen1 = Array2D.length1 grid
    let origLen2 = Array2D.length2 grid
    let mapiFun x y =
        let plusVal = max (x / origLen1) (y / origLen2)
        //printfn "%A" (x / origLen1, y / origLen2)
        //printfn "%i" plusVal
        {coord = (x,y);
         cost = wrap (grid.[x % origLen1, y % origLen2] + plusVal);
         dist = System.Int32.MaxValue;
         prev = None;
         neighbours = Seq.toList (neighbours2 x y grid)}
    let len1 = Array2D.length1 grid * 5
    let len2 = Array2D.length2 grid * 5
    let vArr = Array2D.init len1 len2 mapiFun
    //let vArr = Array2D.mapi mapiFun grid
    vArr.[0,0].dist <- 0
    let target = (Array2D.length1 vArr - 1, Array2D.length2 vArr - 1)

    let vSet =
        vArr
        |> Seq.cast<Node>
        |> Seq.toList
        |> Set.ofList

    (Array2D.map (fun x -> x.cost) vArr, target)
    //(vSet, target)


let findMinDistNode (vSet : Set<Node>) =
    let baseNode = {coord = (-1,-1);
                    cost = System.Int32.MaxValue;
                    dist = System.Int32.MaxValue;
                    prev = None;
                    neighbours = []}
    Set.fold (fun (acc : Node) (x : Node) -> if acc.dist <= x.dist then acc else x) baseNode vSet

let findTarget (vSet : Set<Node>) (target : int * int) =
    let result =
        vSet
        |> Set.filter (fun x -> x.coord = target)
        |> Set.toList
    match result with
    | [] -> failwith "No target vertex found"
    | x::xs -> x

let rec getPathCost (targetNode : Node) : int =
    match targetNode.prev with
    | None -> 0
    | Some node -> targetNode.cost + getPathCost node


let dijkstra (vSet : Set<Node>) (target : int * int) =
    let mutable Q = vSet
    while not (Set.isEmpty Q) do
        let u = findMinDistNode Q
        if u.coord = target then
            Q <- Set.empty
        else
            Q <- Set.remove u Q
            let neighboursInSet =
                Set.filter (fun x -> let coord = x.coord in List.contains coord u.neighbours) Q
            for neighbour in neighboursInSet do
                let (x,y) = neighbour.coord
                let alt = u.dist + neighbour.cost
                if alt < neighbour.dist then
                    neighbour.dist <- alt
                    neighbour.prev <- Some u

    target
    |> findTarget vSet
    |> getPathCost


//let input = readFile "inputSimple.txt"
//let input1, target1 = part1Read <| readFile "inputSimple.txt"
let input2, target2 = part2Read <| readFile "inputSimple.txt"
//printfn "%A" <| findTarget input2 target2
printfn "%A" input2
//printfn "%A" input
//printfn "%A" <| dijkstra input1 target1
//printfn "%A" <| minPath 0 (0,0) input
