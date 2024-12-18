type Direction = | N | East | S | W
type Node = { n : int*int}
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

    let printNodes (size: int) (nodes: Nodes) = 
        [ for y in 0..size-1 do
            for x in 0..size-1 do 
                if (Set.exists (fun n -> (x,y) = n.n) nodes) then 
                    printf "." 
                else printf "#" 
            printfn "" ] |> Seq.toList |> ignore



module Solves =
    open Game
    open Input
    open Xunit


    [<Fact>]
    let test2 () = 
        let input1 = readInit "input1.txt" 
        let input2 = readInit "input2.txt" 
        let nodes1 = generateNodes 7 12 input1
        let nodes2 = generateNodes 71 1024 input2
        printNodes 7 nodes1 
        printNodes 71 nodes2

        Assert.Equal(25, input1.Length) 
        Assert.Equal(3450, input2.Length) 

module Program = let [<EntryPoint>] main _ = 0