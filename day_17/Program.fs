// https://github.com/bjartwolf/algorithms/blob/main/Dijkstra/Program.fs
// E crashses 
type Direction = | N | East | S | W
type Node = { n : int*int*Direction}
type Nodes = Node Set
type NodeHistory = Map<Node,Node>
open FSharpx.Collections
module PQ = PriorityQueue

type Edge = { 
    u : Node;
    v : Node;
    ω_uv : int;
}
type Edges = Edge Set
// https://fsharpforfunandprofit.com/posts/convenience-types/#most-f-types-are-automatically-comparable 
type d_Node = int * Node

let turnLeft (n: Node) : Node =
    match n with
        | { n = (i,j, N) }->  { n = (i,j,W) }
        | { n = (i,j, W) } -> { n = (i,j,S) }
        | { n = (i,j, S) }-> { n =(i,j,East) }
        | { n = (i,j, East) }-> { n=  (i,j,N) }

let turnRight (n: Node) : Node =
    match n with
        | { n = (i,j, N) }->  { n = (i,j,East) }
        | { n = (i,j, East) } -> { n = (i,j,S) }
        | { n = (i,j, S) }-> { n =(i,j,W) }
        | { n = (i,j, W ) }-> { n=  (i,j,N) }

module Maze =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): Edges*Node*(Node list)= 
        let mazeTxt = File.ReadAllLines(filePath) |> Array.toList
        let findStartNode = [for i in 0..mazeTxt.Length-1 do
                                for j in 0..mazeTxt.[i].Length-1 do
                                    if mazeTxt.[i].[j] = 'S' then 
                                        let startNode = {n = (i,j,East) }
                                        yield startNode
                                    else ()
                            ] |> List.head

        let findEndNodes = [for i in 0..mazeTxt.Length-1 do
                                for j in 0..mazeTxt.[i].Length-1 do
                                    if mazeTxt.[i].[j] = 'E' then 
                                        yield {n = (i,j,W) }
                                        yield {n = (i,j,East) }
                                        yield {n = (i,j,N) }
                                        yield {n = (i,j,S) }
                                    else () ] |> List.ofSeq
        let findNodes = [for i in 0..mazeTxt.Length-1 do
                                for j in 0..mazeTxt.[i].Length-1 do
                                    if mazeTxt.[i].[j] = '#' then 
                                        ()
                                    elif mazeTxt.[i].[j] = '.' || mazeTxt.[i].[j] = 'S' then  // counting startnode as a regular node to turn, endnodes we have found
                                        yield {n = (i,j,W) }
                                        yield {n = (i,j,East) }
                                        yield {n = (i,j,N) }
                                        yield {n = (i,j,S) }
                                    else
                                        ()
                             ] |> List.ofSeq
        let allNodes = findNodes @ findEndNodes

        let edges = [ for node in allNodes do
                           // noden kan snu til høyre og venstre
                            yield {u = node; v = turnLeft node; ω_uv = 1000}
                            yield {u = node; v = turnRight node; ω_uv = 1000}
                           // hvis noden har en node med samme retning over i nord, så er det en edge for alle retninger 
                            let connectedNodes = allNodes |> List.filter (fun other-> 
                                                                                  let (n_i,n_j,n_dir) = node.n
                                                                                  let (o_i,o_j,o_dir) = other.n
                                                                                  if (n_i = o_i - 1 && n_j = o_j && n_dir = S && o_dir = S) then true
                                                                                  elif (n_i = o_i + 1 && n_j = o_j && n_dir = N && o_dir = N) then true
                                                                                  elif (n_i = o_i && n_j = o_j + 1 && n_dir = W&& o_dir = W) then true
                                                                                  elif (n_i = o_i && n_j = o_j - 1 && n_dir = East && o_dir = East) then true
                                                                                  else false)
                            let connectedEdges = connectedNodes |> List.map (fun other -> {u = node; v = other; ω_uv = 1})
                            yield! connectedEdges
                            ] |> Set.ofList
        edges, findStartNode, findEndNodes 

    [<Fact>]
    let test2 () = 
        let e, n, _= readInit "input1.txt" 
        Assert.Equal((13,1,East), n.n) 


let updateMapWithSet map updates =  
    updates |> Set.fold (fun accMap (key,value) -> Map.add value key accMap) map

let updatePQWithSet Q updates =  
    updates |> Set.fold (fun q element -> PQ.insert element q) Q 

let adjacent u e = 
    e |> Set.filter (fun edge -> edge.u = u)

let relax (d_u: int) explored vs = 
    let closerOrUndiscovered explored ((dist,n):d_Node) = 
        match Map.tryFind n explored with
            | Some existingDist -> dist < existingDist
            | None -> true
    vs 
      |> Set.map (fun v -> d_u + v.ω_uv, v.v)
      |> Set.filter (fun dn -> closerOrUndiscovered explored dn) 

//printNodes (nodes: )


// Using notation from https://www-student.cse.buffalo.edu/~atri/cse331/support/notation/shortest-path.html
// Somewhat different in my book
// d' is the map from Node to best known upper bound on known nodes at any time,
// d' u would give upper bound to u
// R is the region to explore, R' is the updated region to explore after popping the smallest element
// d_u is the known distance to node u
// s is the source node
// Ε is the set of all edges in the Graph.
[<TailCall>]
let dijkstra Ε s =  
   let rec dijkstra_rec R d' (history: NodeHistory) =  
        match (PQ.tryPop R) with
            | None  -> d', history 
            | Some ((d_u,u), R') ->  
                let relaxed: d_Node Set = adjacent u Ε |> relax d_u d' 
                let relaxedNodes = relaxed |> Set.map (fun (_,x) -> u,x)
                let history' = updateMapWithSet history relaxedNodes
                dijkstra_rec (updatePQWithSet R' relaxed) 
                             (updateMapWithSet d' relaxed)
                             history'
   let pq = PQ.empty false |> PQ.insert (0, s)
   dijkstra_rec pq (Map.ofList [s, 0]) Map.empty


let rec printHistory (startNode: Node ) (history: NodeHistory) = 
    printfn "%A" startNode
    let next = Map.tryFind startNode history
    match next with
        | None -> printfn "start"
        | Some node -> printHistory node (history |> Map.remove startNode)
module Program = 
    open System
    open Maze 
    let [<EntryPoint>] main _ = 
        let edges, start, endNodes = readInit "input1.txt"
        let solution,history = dijkstra edges start
        printfn "history: %A" history 
        let value, shortestNode = endNodes |> List.map (fun n -> Map.find n solution, n) |> List.min
        printfn "%A" value 
        printHistory shortestNode history 
        Console.ReadKey() |> ignore
        0