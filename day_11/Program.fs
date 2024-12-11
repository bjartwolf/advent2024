module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): int list = 
        let numbers = File.ReadAllText(filePath).Split " " 
        numbers |> Array.map(fun f -> Int32.Parse(f)) |> Array.toList

    let runGeneration (input: int list): int list = 
        [
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
                    yield firstHalf |> int 
                    yield secondHalf |> int 
                else 
                    yield num * 2024 
        ]

    [<Fact>]
    let runGen () = 
        Assert.Equivalent([253000], runGeneration [125])
        Assert.Equivalent([253000;1;7], runGeneration [125;17])
 
    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(2, input.Length) 

        let input = readInit "input2.txt" 
        Assert.Equal(8, input.Length) 

module Program = let [<EntryPoint>] main _ = 0