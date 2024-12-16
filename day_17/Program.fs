// https://github.com/bjartwolf/algorithms/blob/main/Dijkstra/Program.fs
// E crashses 
type Direction = | N | East | S | W
type Node = { n : int*int*Direction}
type Nodes = Node Set
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
                                    if mazeTxt.[i].[j] = 'S' then 
                                        yield {n = (i,j,W) }
                                        yield {n = (i,j,East) }
                                        yield {n = (i,j,N) }
                                        yield {n = (i,j,S) }
                                    else ()
                            ] 
        Set.empty, findStartNode, [{n = 0,0,S};{n = 0,0,S} ]

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
   let rec dijkstra_rec R d' =  
        match (PQ.tryPop R) with
            | None  -> d' 
            | Some ((d_u,u), R') ->  
                let relaxed: d_Node Set = adjacent u Ε 
                                            |> relax d_u d' 
                dijkstra_rec (updatePQWithSet R' relaxed) 
                             (updateMapWithSet d' relaxed)
   let pq = PQ.empty false |> PQ.insert (0, s)
   dijkstra_rec pq (Map.ofList [s, 0]) 



module Program = let [<EntryPoint>] main _ = 0