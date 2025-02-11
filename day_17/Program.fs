﻿// https://github.com/bjartwolf/algorithms/blob/main/Dijkstra/Program.fs
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
            | Some existingDist -> dist <= existingDist && dist <= 85420
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
let mutable history : Map<Node, int*(Node list)> = Map.empty
[<TailCall>]
let dijkstra Ε s =  
   let rec dijkstra_rec R d'  =  
        match (PQ.tryPop R) with
            | None  -> d'
            | Some ((d_u,u), R') ->  
                let relaxed: d_Node Set = adjacent u Ε |> relax d_u d' 
                // all the nodes that are found now we can add to history or replace history
                // we look at all the new nodes 
                for (cost,node) in relaxed do
                    let his = history |> Map.tryFind node 
                    if cost <= 85420 then
                        match his with
                            | None -> history <- history |> Map.add node (cost, [u]) 
                            | Some (histCost,histNodes) -> if histCost > cost then
                                                                history <- history |> Map.add node (cost, [u]) 
                                                           elif histCost < cost then
                                                                ()
                                                           else
                                                                history <- history |> Map.add node (cost, (u::histNodes) |> List.distinct) 
                dijkstra_rec (updatePQWithSet R' relaxed) 
                             (updateMapWithSet d' relaxed)
   let pq = PQ.empty false |> PQ.insert (0, s)
   dijkstra_rec pq (Map.ofList [s, 0]) 


let mutable i = 0
let rec printHistory (startNode: Node ) (history: Map<Node,(int*Node list)>) : Node seq = 
    [
    i <- i + 1
//    printfn "%A" startNode
    let next = Map.tryFind startNode history
    match next with
        | None -> 
            i <-i+1
//            printfn "start %A" (i)
        | Some (x, []) -> 
                         yield startNode
                         yield! printHistory startNode (Map.remove startNode history)
        | Some (x, node :: rest) -> if rest <> [] then yield! printHistory (rest.Head) history 
                                    yield node
                                    yield! printHistory node history 
    ]

let printNodes (nodes: Node list) = 
    let maxX = nodes |> List.map (fun n -> let (i,j,_) = n.n in i) |> List.max 
    let maxY = nodes |> List.map (fun n -> let (i,j,_) = n.n in i) |> List.max 
    [
        for x in 0..maxX do 
            for y in 0..maxX do 
                if nodes |> List.exists (fun n -> let (i,j,_) = n.n in i = x && j = y) then 
                    printf "#"
                else
                    printf " "
            printfn ""

    ] |> Seq.toList |> ignore

    ()
module Program = 
    open System
    open Maze 
    let [<EntryPoint>] main _ = 
        let edges, start, endNodes = readInit "input2.txt"
        let solution= dijkstra edges start
        let value, shortestNode = endNodes |> List.map (fun n -> Map.find n solution, n) |> List.min
        printfn "%A" value 
//        printfn "%A" history
        let history = printHistory shortestNode history |> Seq.toList
        printNodes history
        printfn "total history %A" (history |> List.distinctBy (fun n -> let i,j,_ = n.n
                                                                         (i,j))  |> List.length)
        Console.ReadKey() |> ignore
        0