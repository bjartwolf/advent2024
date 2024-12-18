type Direction = | N | East | S | W
type Node = { n : int*int}
type Nodes = Node Set
type NodeHistory = Map<Node,Node>
open FSharpx.Collections
open Xunit
module PQ = PriorityQueue

type Edge = { 
    u : Node;
    v : Node;
    ω_uv : int;
}
type Edges = Edge Set
// https://fsharpforfunandprofit.com/posts/convenience-types/#most-f-types-are-automatically-comparable 
type d_Node = int * Node

module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): (int*int) list = 
        let lines = File.ReadAllLines(filePath) 
        lines |> Array.map(fun f -> 
            let parts = f.Split(",")
            Int32.Parse(parts.[0]),Int32.Parse(parts.[1]))
            |> Array.toList
module Game =
    let generateNodes (size:int) (nrDropped:int) (corrupted: (int*int) list): Nodes= 
        [ for x in 0..size-1 
            do for y in 0..size-1 do 
                yield (x,y) ] |> List.except (corrupted |> (List.take nrDropped))|> List.map (fun (x,y) -> {n = (x,y)} ) |> Set.ofList
    let generateEdges (nodes: Nodes) : Edges= 
        let dist (n: Node) (n': Node) = 
            let (x,y) = n.n
            let (x',y') = n'.n
            let dx = abs(x - x')
            let dy = abs(y - y')
            dx + dy 
        [ for n in nodes do
                for n' in nodes do
                    if dist n n' = 1 then 
                        yield {u = n; v = n'; ω_uv = 1} ] |> Set.ofSeq

    let printNodes (size: int) (nodes: Nodes) = 
        [ for y in 0..size-1 do
            for x in 0..size-1 do 
                if (Set.exists (fun n -> (x,y) = n.n) nodes) then 
                    printf "." 
                else printf "#" 
            printfn "" ] |> Seq.toList |> ignore



module Solver = 
    open Game
    open Input
    open Xunit
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

    //https://web.engr.oregonstate.edu/~glencora/wiki/uploads/dijkstra-proof.pdf

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
                    let relaxed: d_Node Set = adjacent u Ε |> relax d_u d' 
                    dijkstra_rec (updatePQWithSet R' relaxed) 
                                 (updateMapWithSet d' relaxed)
       let pq = PQ.empty false |> PQ.insert (0, s)
       dijkstra_rec pq (Map.ofList [s, 0]) 


    let distanceToEndNode (size: int) (fileName: string) (nrOfBits:int) : int option =
        let nodes = generateNodes size nrOfBits (readInit fileName)
        let edges = generateEdges nodes
        let solved = dijkstra edges { n= (0,0)}
        solved |> Map.tryFind {n = (size-1,size-1)}

    [<Fact>]
    let test2 () = 
        let input1 = readInit "input1.txt" 
        let nodes1 = generateNodes 7 12 input1
        printNodes 7 nodes1 
        let distanceToEnd = distanceToEndNode 7 "input1.txt" 12
        Assert.Equal(Some 22, distanceToEnd)
        let distanceToEnd = distanceToEndNode 71 "input2.txt" 1024 
        Assert.Equal(Some 320, distanceToEnd)
        let input2 = readInit "input2.txt" 
        let nodes2 = generateNodes 71 1024 input2
        printNodes 71 nodes2
        Assert.Equal(3450, input2.Length) 

        Assert.Equal(25, input1.Length) 

module Program = let [<EntryPoint>] main _ = 0