module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): int64 list = 
        let numbers = File.ReadAllText(filePath).Split " " 
        numbers |> Array.map(fun f -> Int64.Parse(f)) |> Array.toList

    let runGeneration (input: int64 list): int64 list = 
        [
            try 
                for num in input do
                    if num = 0 then
                        yield 1 
                    else if num.ToString().Length % 2 = 0 then
                        let numStr = num.ToString()
                        let half =numStr.Length / 2
//                        printfn "num: %A halfLeng %A length %A" numStr half numStr.Length
                        let firstHalf = numStr.Substring(0, half)
                        let secondHalf = numStr.Substring(half,  half)
 //                       printfn "firstHalf: %A secondHalf: %A" firstHalf secondHalf
                        yield firstHalf |> int64
                        yield secondHalf |> int64
                    else 
                        yield num * 2024L 
            with 
                | e -> printfn "Error: %A" e.Message
        ]
    // før eller siden blir alle tallene 0 igjen og da utvikler de seg i et mønster 
    let runGenerationN (input: int64 list) (gen: int): (int64*int option) list = 
        [
            try 
                for num in input do
                    if num = 0 then
                        yield (1, Some (gen - 1))
                    else if num.ToString().Length % 2 = 0 then
                        let numStr = num.ToString()
                        let half =numStr.Length / 2
//                        printfn "num: %A halfLeng %A length %A" numStr half numStr.Length
                        let firstHalf = numStr.Substring(0, half)
                        let secondHalf = numStr.Substring(half,  half)
 //                       printfn "firstHalf: %A secondHalf: %A" firstHalf secondHalf
                        let first = firstHalf |> int64
                        let second = secondHalf |> int64
                        if (first = 0) then 
                            yield (0, Some gen)
                        else 
                            yield (first, None)
                        if (second= 0) then 
                            yield (0, Some gen)
                        else 
                            yield (second, None)
                     else 
                        yield (num * 2024L, None)
            with 
                | e -> printfn "Error: %A" e.Message
        ]

    let runGenNTimes (input: int64 list) (n: int): int64 list = 
        let mutable result = input
        let mutable prevGen = 1
        for i in 1..n do
            printfn "Unique numbers in generation %A is %A" (i-1) (result |> List.distinct |> List.length)
 //           printfn "Generation %A is %A with diff %A " (i-1) result.Length (result.Length - prevGen)
//            printfn "Generation %A is %A " (i-1) result.Length 
//            printfn "Generation %A is %A " (i-1) result
            prevGen <- result.Length
            result <- runGeneration result
        result

    let runGenNTimesN (input: int64 list) (n: int): int64 list = 
        let mutable result = input
        let mutable reachedZero: int list = [] 
        let mutable prevGen = 1
        for i in 1..n do
            //printfn "Generation %A is %A with diff %A " (i-1) result.Length (result.Length - prevGen)
//            printfn "Generation %A is %A " (i-1) result.Length 
            prevGen <- result.Length
            let nextRes  = runGenerationN result i 
            let nextGen = nextRes |> List.map fst
            let newZeros = nextRes |> List.map snd |> List.choose id 
            result <- nextGen |> List.filter (fun x -> x <>0)
            reachedZero <- reachedZero @ newZeros
            printfn "Generation %A is %A and reached zero is %A" (i-1) result.Length reachedZero.Length
        result


    [<Fact>]
    let runGen () = 
        Assert.Equivalent([253000], runGeneration [125])
        Assert.Equivalent([253000;1;7], runGeneration [125;17])
        Assert.Equivalent([253;0;2024;14168], runGeneration [125;17] |> runGeneration)
 
    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(2, input.Length) 
        Assert.Equivalent([2097446912;14168;4048;2;0;2;4;40;48;2024;40;48;80;96;2;8;6;7;6;0;3;2], runGenNTimes input 6)
        Assert.Equivalent(55312, runGenNTimes input 25 |> List.length)

        let input = readInit "input2.txt" 
        Assert.Equal(8, input.Length) 

        Assert.Equivalent(239714, runGenNTimes input 25 |> List.length)

open Input
open System

module Program = 
    let [<EntryPoint>] main _ = 
        let input = readInit "input2.txt" 
        //let answer = runGenNTimesN input 75 |> List.length
        let answer = runGenNTimes input 30 |> List.length
        //printfn "%A" answer
        (*
        let answer = runGenNTimes input 25 |> List.length
        printfn "%A" answer
        *)
//
        let input = readInit "input1.txt" 
//        let answer = runGenNTimes input 25 |> List.length
//        printfn "%A" answer
//        let answer = runGenNTimes input 75 |> List.length
//        printfn "%A" answer
        0