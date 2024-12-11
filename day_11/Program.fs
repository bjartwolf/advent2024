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
                        printfn "num: %A halfLeng %A length %A" numStr half numStr.Length
                        let firstHalf = numStr.Substring(0, half)
                        let secondHalf = numStr.Substring(half,  half)
                        printfn "firstHalf: %A secondHalf: %A" firstHalf secondHalf
                        yield firstHalf |> int64
                        yield secondHalf |> int64
                    else 
                        yield num * 2024L 
            with 
                | e -> printfn "Error: %A" e.Message
        ]
    let runGenNTimes (input: int64 list) (n: int): int64 list = 
        let mutable result = input
        for i in 1..n do
            result <- runGeneration result
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

        Assert.Equivalent(239914, runGenNTimes input 25 |> List.length)
module Program = let [<EntryPoint>] main _ = 0