open System.Collections.Generic

module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): int64 list = 
        let numbers = File.ReadAllText(filePath).Split " " 
        numbers |> Array.map(fun f -> Int64.Parse(f)) |> Array.toList

    let runGeneration1 (input: int64 list): (int64*int64) list = 
        [
            try 
                for num in input do
                    if num = 0 then
                        yield (1L,1L) 
                    else if num.ToString().Length % 2 = 0 then
                        let numStr = num.ToString()
                        let half =numStr.Length / 2
//                        printfn "num: %A halfLeng %A length %A" numStr half numStr.Length
                        let firstHalf = numStr.Substring(0, half)
                        let secondHalf = numStr.Substring(half,  half)
 //                       printfn "firstHalf: %A secondHalf: %A" firstHalf secondHalf
                        yield (firstHalf |> int64,1L)
                        yield (secondHalf |> int64,1L)
                    else 
                        yield (num * 2024L,1L)
            with 
                | e -> printfn "Error: %A" e.Message
        ]
    let runGenerationN (input: (int64*int64) list): (int64*int64) list = 
        [
            try 
                for num, count in input do
                    if num = 0 then
                        yield (1L,count) 
                    else if num.ToString().Length % 2 = 0 then
                        let numStr = num.ToString()
                        let half =numStr.Length / 2
//                        printfn "num: %A halfLeng %A length %A" numStr half numStr.Length
                        let firstHalf = numStr.Substring(0, half)
                        let secondHalf = numStr.Substring(half,  half)
 //                       printfn "firstHalf: %A secondHalf: %A" firstHalf secondHalf
                        yield (firstHalf |> int64,count)
                        yield (secondHalf |> int64,count)
                    else 
                        yield (num * 2024L,count)
            with 
                | e -> printfn "Error: %A" e.Message
        ] 
        
    // input is a list of pairs of number and counts
    // should return the same list but with the unique numbers and counts 
    let sumCounts (input: (int64*int64) list): (int64*int64) list = 
        let mutable counts : Dictionary<int64,int64> = Dictionary<int64, int64> ()
        for (number,count) in input do
                let inList,sum = counts.TryGetValue(number)
                if inList then 
                    counts.[number] <- sum + count 
                else
                    counts.[number] <- count 
        // convert counts to F# list 
        counts |> Seq.map (fun k -> (k.Key,k.Value)) |> List.ofSeq

    [<Fact>]
    let sumCountstest () = 
        Assert.Equivalent([(2,3)], sumCounts [(2,1);(2,2)])
        Assert.Equivalent([(1,1);(2,1)], sumCounts [(1,1);(2,1)])
        Assert.Equivalent([(1,3)], sumCounts [(1,1);(1,1);(1,1)])
 
    // før eller siden blir alle tallene 0 igjen og da utvikler de seg i et mønster 
    let runGenNTimesN (input: int64 list) (n: int): int64 list = 
        let mutable result = input |> runGeneration1
        for i in 2..(n-1) do
            result <- runGenerationN result |> sumCounts
        let foo = result |> sumCounts 
        foo |> List.map (fun (num,count) -> count)


 
    [<Fact>]
    let test2 () = 
        let input = readInit "input2.txt" 
        Assert.Equivalent(284973560658514L, runGenNTimesN input 76 |> List.sum)
        Assert.Equivalent(4, runGenNTimesN input 3 |> List.sum)
        Assert.Equivalent(55312, runGenNTimesN input 26 |> List.sum)
//        Assert.Equivalent(22, runGenNTimesN input 6 |> List.sum)

 //       Assert.Equal(8, input.Length) 
  //      Assert.Equivalent(239714, runGenNTimesN input 25 |> List.sum)

open Input
open System

module Program = 
    let [<EntryPoint>] main _ = 
        let input = readInit "input1.txt" 
        //let answer = runGenNTimesN input 75 |> List.length
        let answer = runGenNTimesN input 5 
        printfn "%A" answer
        Console.ReadKey() |> ignore
        (*
        let answer = runGenNTimes input 25 |> List.length
        printfn "%A" answer
        *)
//
//        let input = readInit "input1.txt" 
//        let answer = runGenNTimes input 25 |> List.length
//        printfn "%A" answer
//        let answer = runGenNTimes input 75 |> List.length
//        printfn "%A" answer
        0