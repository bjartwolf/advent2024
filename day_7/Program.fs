module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): (int*(int list)) list = 
        let parseLine (line: string): int list = 
            line.Split(" ") 
                 |> Array.where(fun s -> String.IsNullOrEmpty(s) |> not)
                 |> Array.map (fun n -> Int32.Parse(n))
                 |> Array.toList

        File.ReadAllLines(filePath) |> Array.toList
            |> List.map (fun line -> line.Split(":"))
            |> List.map (fun parts -> parts.[0] |> int, parseLine parts.[1]) 

    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        printfn "%A" input
        Assert.Equal(9, input.Length) 

module Program = let [<EntryPoint>] main _ = 0